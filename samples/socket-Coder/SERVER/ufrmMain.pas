unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, uIOCPCentre, ExtCtrls,
  ComObj;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    pnlMonitor: TPanel;
    actPushMsg: TAction;
    edtMsg: TEdit;
    btnPushMsg: TButton;
    procedure actOpenExecute(Sender: TObject);
    procedure actPushMsgExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TIOCPConsole;
    procedure refreshState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnRecvObject(pvClientContext:TIocpClientContext;pvObject:TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, uIOCPJSonStreamEncoder, uIOCPJSonStreamDecoder, JSonStream;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TIOCPConsole.Create(Self);
  FTcpServer.createDataMonitor;
  FTcpServer.OnDataObjectReceived := OnRecvObject;
  FTcpServer.registerCoderClass(TIOCPJSonStreamDecoder, TIOCPJSonStreamEncoder);
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
end;

destructor TfrmMain.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmMain.OnRecvObject(pvClientContext: TIocpClientContext;
  pvObject: TObject);
begin
  pvClientContext.writeObject(pvObject);
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
  FTcpServer.Active := true;
  refreshState;
end;

procedure TfrmMain.actPushMsgExecute(Sender: TObject);
var
  lvList:TList;
  i: Integer;
  lvMsg:TJSonStream;
begin
  lvList := TList.Create;
  try
    lvMsg := TJsonStream.Create;
    try
      lvMsg.Json.S['id'] := CreateClassID;
      lvMsg.Json.S['info'] := 'diocp3 push message';
      lvMsg.Json.S['severtime'] := FormatDateTime('yyyy-MM-dd hh:nn:ss', Now());
      lvMsg.Json.S['msg'] := edtMsg.Text;
      FTcpServer.getOnlineContextList(lvList);
      for i := 0 to lvList.Count-1 do
      begin
        TIOCPClientContext(lvList[i]).writeObject(lvMsg);
      end;
    finally
      lvMsg.Free;
    end;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

end.
