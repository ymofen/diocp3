unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpCoderClientSocket,
  uDIOCPStreamCoder, iocpBaseSocket, safeLogger;

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
    FContext:TIocpCoderRemoteContext;
    FiocpCoderClient: TIocpCoderClient;

    procedure OnRecvObject(pvObject:TObject);

    procedure OnDisconnected(pvObject: TIocpBaseContext);
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
  
  FiocpCoderClient := TIocpCoderClient.Create(Self);
  FContext := TIocpCoderRemoteContext(FiocpCoderClient.Add);
  FiocpCoderClient.OnContextDisconnected := OnDisconnected;

  FContext.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FContext.OnDataObjectReceived := OnRecvObject;
end;

destructor TfrmMain.Destroy;
begin
  sfLogger.Enable :=false;
  inherited;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  if FContext.Active then
  begin
    sfLogger.logMessage('already connected...');
    Exit;
  end;

  FiocpCoderClient.open; 

  FContext.Host := edtHost.Text;
  FContext.Port := StrToInt(edtPort.Text);
  FContext.connectASync;

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
    s := 'this message will send to server';
    lvStream.Write(s[1], Length(s));

    lvStream.Position := 0;

    //send stream object
    FContext.writeObject(lvStream);
  finally
    lvStream.Free;
  end;

end;

procedure TfrmMain.OnDisconnected(pvObject: TIocpBaseContext);
begin
  sfLogger.logMessage('disconnected');
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

  sfLogger.logMessage('recv msg from server:' + sLineBreak + '    ' + s);
  sfLogger.logMessage('');

end;

end.
