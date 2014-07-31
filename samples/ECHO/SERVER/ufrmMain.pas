unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, iocpTcpServer, ExtCtrls,
  iocpUILogger,
  System.Actions;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    pnlMonitor: TPanel;
    Button1: TButton;
    Button2: TButton;
    mmoLog: TMemo;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  FTcpServer.SetWorkerCount(2);
  FTcpServer.Active := true;
  refreshState;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  FTcpServer.DisConnectAll();
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  lvLink:TIocpRequestSingleLink;
  lvRequest:TIocpSendRequest;
  i: Integer;
begin
  lvLink := TIocpRequestSingleLink.Create(20000);

  for i := 0 to 10000 do
  begin
    lvRequest := TIocpSendRequest.Create;
    lvLink.Push(lvRequest);
  end;


  for i := 0 to 10000 do
  begin
    lvRequest := TIocpSendRequest(lvLink.Pop);
    lvRequest.Free;
  end;

  lvLink.Free;




end;

procedure TfrmMain.OnRecvBuffer(pvClientContext:TIocpClientContext;
    buf:Pointer; len:cardinal; errCode:Integer);
begin
  if errCode = 0 then
  begin
    pvClientContext.PostWSASendRequest(buf, len);
  end else
  begin
    pvClientContext.DoDisconnect;
  end;
end;

end.
