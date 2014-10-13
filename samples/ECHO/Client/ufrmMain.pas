unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpClientSocket,
  iocpLogger, ComCtrls, iocpBaseSocket;

type
  TfrmMain = class(TForm)
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    tsMonitor: TTabSheet;
    mmoRecvMessage: TMemo;
    tsOperator: TTabSheet;
    mmoData: TMemo;
    btnClose: TButton;
    btnCreate: TButton;
    edtCount: TEdit;
    chkSendData: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure chkSendDataClick(Sender: TObject);
  private
    FSendDataOnConnected:Boolean;
    { Private declarations }
    FIocpClientSocket: TIocpClientSocket;

    procedure OnContextConnected(pvContext: TIocpBaseContext);

    procedure OnRecvdBuffer(pvContext: TIocpBaseContext; buf: Pointer; len:
        cardinal; pvErrorCode: Integer);

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor;
{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FIocpClientSocket := TIocpClientSocket.Create(Self);
  FIocpClientSocket.createDataMonitor;
  FIocpClientSocket.OnContextConnected := OnContextConnected;
  FIocpClientSocket.OnReceivedBuffer := OnRecvdBuffer;
  TFMMonitor.createAsChild(tsMonitor, FIocpClientSocket);

  uiLogger.setLogLines(mmoRecvMessage.Lines);


end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSendDataOnConnected := true;
end;

destructor TfrmMain.Destroy;
begin
  FIocpClientSocket.Close;
  FIocpClientSocket.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FIocpClientSocket.Count-1 do
  begin
    FIocpClientSocket.Items[i].AutoReConnect := false;
    FIocpClientSocket.Items[i].Close;

  end;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
var
  lvClient:TIocpRemoteContext;
begin
  FIocpClientSocket.open;

  lvClient := FIocpClientSocket.Add;
  lvClient.Host := edtHost.Text;
  lvClient.Port := StrToInt(edtPort.Text);
  lvClient.AutoReConnect := true;
  lvClient.connectASync;



  mmoRecvMessage.Clear;

  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnCreateClick(Sender: TObject);

var
  lvClient:TIocpRemoteContext;
  i:Integer;

begin
  FIocpClientSocket.open;

  for i := 1 to StrToInt(edtCount.Text) do
  begin
    lvClient := FIocpClientSocket.Add;
    lvClient.Host := edtHost.Text;
    lvClient.Port := StrToInt(edtPort.Text);
    lvClient.AutoReConnect := true;
    lvClient.connectASync;
  end;

end;

procedure TfrmMain.chkSendDataClick(Sender: TObject);
begin
  FSendDataOnConnected := chkSendData.Checked;
end;

procedure TfrmMain.OnContextConnected(pvContext: TIocpBaseContext);
var
  s:AnsiString;
begin
  if FSendDataOnConnected then
  begin
    s := Trim(mmoData.Lines.Text);

    pvContext.PostWSASendRequest(PAnsiChar(s), Length(s));
  end;

end;

procedure TfrmMain.OnRecvdBuffer(pvContext: TIocpBaseContext; buf: Pointer;
    len: cardinal; pvErrorCode: Integer);
begin
  if len = 0 then
  begin
    uiLogger.logMessage('recv err zero');
  end;
  if pvErrorCode = 0 then
  begin
    Sleep(10);
    pvContext.PostWSASendRequest(buf, len);
  end else
  begin
    uiLogger.logMessage('recv err:%d', [pvErrorCode]);
  end;
end;

end.
