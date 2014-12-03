unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, RawTcpClient,
  iocpLogger;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendBuf: TButton;
    btnOnlySend: TButton;
    mmoData: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure btnOnlySendClick(Sender: TObject);
    procedure btnSendBufClick(Sender: TObject);
  private
    { Private declarations }
    FTcpClient:TRawTcpClient;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation


{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  uiLogger.setLogLines(mmoRecvMessage.Lines);
  FTcpClient := TRawTcpClient.Create(Self);
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  if FTcpClient.Active then
  begin
    uiLogger.logMessage('already connected...');
    Exit;
  end;
  FTcpClient.Host := edtHost.Text;
  FTcpClient.Port := StrToInt(edtPort.Text);
  FTcpClient.Connect;
  
  mmoRecvMessage.Clear;
  mmoRecvMessage.Lines.Add('connected');
  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnOnlySendClick(Sender: TObject);
var
  rcvs, s:AnsiString;
begin
  try
    s := mmoData.Lines.Text;
    FTcpClient.sendBuffer(@s[1],Length(s));
  except
    FTcpClient.Disconnect;
    mmoRecvMessage.Lines.Add('disconnected');
    raise;
  end;



end;

procedure TfrmMain.btnSendBufClick(Sender: TObject);
var
  rcvs, s:AnsiString;


  i:Integer;
begin
  s := 'this message will send to server';
  FTcpClient.sendBuffer(@s[1],Length(s));

  SetLength(rcvs, 1024);

  i := FTcpClient.RecvBuffer(@rcvs[1], 1024);

  SetLength(rcvs, i);

  mmoRecvMessage.Lines.Add('接受到服务端数据:' + rcvs);


end;

end.
