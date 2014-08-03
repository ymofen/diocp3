unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpCoderTcpClient,
  iocpLogger, uIOCPJSonStreamDecoder, uIOCPJSonStreamEncoder;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
  private
    { Private declarations }
    FiocpCoderTcpClient:TiocpCoderTcpClient;

    procedure OnRecvObject(pvObject:TObject);

    procedure OnDisconnected(pvObject:TObject);
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JSonStream;



{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  uiLogger.setLogLines(mmoRecvMessage.Lines);
  FiocpCoderTcpClient := TiocpCoderTcpClient.Create(Self);
  FiocpCoderTcpClient.registerCoderClass(TIOCPJSonStreamDecoder, TIOCPJSonStreamEncoder);
  FiocpCoderTcpClient.OnDataObjectReceived := OnRecvObject;
  FiocpCoderTcpClient.OnDisconnected := OnDisconnected;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  if FiocpCoderTcpClient.isActive then
  begin
    uiLogger.logMessage('already connected...');
    Exit;
  end;
  FiocpCoderTcpClient.Host := edtHost.Text;
  FiocpCoderTcpClient.Port := StrToInt(edtPort.Text);
  FiocpCoderTcpClient.Connect;

  mmoRecvMessage.Clear;

  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnSendObjectClick(Sender: TObject);
begin
  ;
end;

procedure TfrmMain.OnDisconnected(pvObject: TObject);
begin
  uiLogger.logMessage('disconnected');
end;

procedure TfrmMain.OnRecvObject(pvObject: TObject);
begin
  uiLogger.logMessage(TJsonStream(pvObject).Json.AsJSon(true, False));

end;

end.
