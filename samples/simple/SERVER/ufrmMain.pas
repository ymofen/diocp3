unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, iocpTcpServer, ExtCtrls,
  iocpLogger, ComCtrls;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    btnDisconectAll: TButton;
    pgcMain: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    mmoLog: TMemo;
    pnlMonitor: TPanel;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TIocpTcpServer;
    procedure refreshState;
    procedure OnRecvBuffer(pvClientContext:TIocpClientContext; buf:Pointer;
        len:cardinal; errCode:Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, iocpEngine;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TIocpTcpServer.Create(Self);
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.createDataMonitor;
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);

  uiLogger.setLogLines(mmoLog.Lines);
end;

destructor TfrmMain.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmMain.refreshState;
begin
  if FTcpServer.Active then
  begin
    btnOpen.Action := actStop;
  end else
  begin
    btnOpen.Action := actOpen;
  end;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.Port := StrToInt(edtPort.Text);
 // FTcpServer.SetWorkerCount(2);
  FTcpServer.Active := true;
  refreshState;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

procedure TfrmMain.btnDisconectAllClick(Sender: TObject);
begin
  FTcpServer.DisConnectAll();
end;

procedure TfrmMain.OnRecvBuffer(pvClientContext:TIocpClientContext;
    buf:Pointer; len:cardinal; errCode:Integer);
var
  s:AnsiString;
begin
  if errCode = 0 then
  begin
    SetLength(s, len);
    Move(buf^, s[1], len);
    uiLogger.logMessage( '接收到客户端[' + pvClientContext.RemoteAddr + ']数据:' + s);
    pvClientContext.PostWSASendRequest(buf, len);
  end else
  begin
    pvClientContext.DoDisconnect;
  end;
end;

end.
