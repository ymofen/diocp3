unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpCoderTcpClient,
  iocpLogger, uDIOCPStreamCoder;

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





{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  uiLogger.setLogLines(mmoRecvMessage.Lines);
  FiocpCoderTcpClient := TiocpCoderTcpClient.Create(Self);
  FiocpCoderTcpClient.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
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
var
  lvList:TList;
  i: Integer;
  lvStream:TMemoryStream;
  s:AnsiString;
begin
  lvStream := TMemoryStream.Create;
  try
    lvStream.LoadFromFile('C:\1.txt');
//    s := 'this message will send to server';
//    lvStream.Write(s[1], Length(s));
//
//    lvStream.Position := 0;

    //send stream object
    FiocpCoderTcpClient.writeObject(lvStream);
  finally
    lvStream.Free;
  end;

end;

procedure TfrmMain.OnDisconnected(pvObject: TObject);
begin
  uiLogger.logMessage('disconnected');
end;

procedure TfrmMain.OnRecvObject(pvObject: TObject);
var
  lvStream:TMemoryStream;
  s:AnsiString;
begin
  // recv stream object
  lvStream := TMemoryStream(pvObject);


  SetLength(s, lvStream.Size);
  lvStream.Position := 0;
  lvStream.Read(s[1], lvStream.Size);

  uiLogger.logMessage('recv msg from server:' + sLineBreak + '    ' + s);
  uiLogger.logMessage('');

end;

end.
