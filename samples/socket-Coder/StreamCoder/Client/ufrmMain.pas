unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpCoderClient,
  safeLogger, 
  iocpLogger, uDIOCPDxStreamCoder, iocpTask, iocpBaseSocket;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    mmoData: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
  private
    { Private declarations }
    FIocpClient:TIocpCoderRemoteContext;
    FiocpCoderTcpClient:TIocpCoderClient;

    procedure OnRecvObject(pvObject:TObject);

    procedure OnDisconnected(pvContext: TIocpBaseContext);
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation


{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  sfLogger.setAppender(TStringsAppender.Create(mmoRecvMessage.Lines));
  sfLogger.AppendInMainThread := true;
  FiocpCoderTcpClient := TIocpCoderClient.Create(Self);
  FIocpClient :=TIocpCoderRemoteContext(FiocpCoderTcpClient.Add);

  FIocpClient.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FIocpClient.OnDataObjectReceived := OnRecvObject;
  FiocpCoderTcpClient.OnContextDisconnected := OnDisconnected;


end;

destructor TfrmMain.Destroy;
begin
  sfLogger.Enable := false;
  FiocpCoderTcpClient.DisconnectAll;
  FiocpCoderTcpClient.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FiocpCoderTcpClient.open;
  
  if FIocpClient.Active then
  begin
    uiLogger.logMessage('already connected...');
    Exit;
  end;
  FIocpClient.Host := edtHost.Text;
  FIocpClient.Port := StrToInt(edtPort.Text);
  FIocpClient.Connect;

  mmoRecvMessage.Clear;

  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnSendObjectClick(Sender: TObject);
var
  lvStream:TMemoryStream;
  s:AnsiString;
begin
  lvStream := TMemoryStream.Create;
  try
   // lvStream.LoadFromFile('C:\1.txt');
    s := mmoData.Lines.Text;
    lvStream.Write(s[1], Length(s));

    lvStream.Position := 0;

    //send stream object
    FIocpClient.writeObject(lvStream);
  finally
    lvStream.Free;
  end;

end;

procedure TfrmMain.OnDisconnected(pvContext: TIocpBaseContext);
begin
  if csDestroying in ComponentState then
  begin
    exit;
  end;

  uiLogger.logMessage('disconnected');
end;

procedure TfrmMain.OnRecvObject(pvObject: TObject);
var
  s:AnsiString;
  lvStream:TMemoryStream;
begin
  lvStream := TMemoryStream(pvObject);
  SetLength(s, lvStream.Size);
  lvStream.Position := 0;
  lvStream.Read(s[1], lvStream.Size);

  sfLogger.logMessage('recv msg from server:' + sLineBreak + '    ' + s);
  sfLogger.logMessage('');
 end;

end.
