unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpCoderTcpClient,
  iocpLogger, uDIOCPStreamCoder, IocpFileASyncTrans, iocpTask;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    btnGetFile: TButton;
    edtFileID: TEdit;
    procedure btnConnectClick(Sender: TObject);
    procedure btnGetFileClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
  private
    { Private declarations }
    FiocpCoderTcpClient:TiocpCoderTcpClient;

    //
    FFileAsyncTrans: TIocpFileASyncTrans;

    procedure OnRecvObject(pvObject:TObject);

    procedure OnDisconnected(pvObject:TObject);
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  FileTransProtocol;





{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  uiLogger.setLogLines(mmoRecvMessage.Lines);

  FFileAsyncTrans := TIocpFileASyncTrans.Create;
  FiocpCoderTcpClient := TiocpCoderTcpClient.Create(Self);
  FiocpCoderTcpClient.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FiocpCoderTcpClient.OnDataObjectReceived := OnRecvObject;
  FiocpCoderTcpClient.OnDisconnected := OnDisconnected;
  FFileAsyncTrans.IocpTcpCoderTcpClient := FiocpCoderTcpClient;


end;

destructor TfrmMain.Destroy;
begin
  FiocpCoderTcpClient.Disconnect;
  FiocpCoderTcpClient.Free;
  FFileAsyncTrans.Free;
  inherited Destroy;
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

procedure TfrmMain.btnGetFileClick(Sender: TObject);
begin
  if not FiocpCoderTcpClient.isActive then
  begin
    uiLogger.logMessage('please do connect');
    exit;
  end;

  FFileAsyncTrans.requestFileINfo(edtFileID.Text);
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
   // lvStream.LoadFromFile('C:\1.txt');
    s := 'this message will send to server';
    lvStream.Write(s[1], Length(s));

    lvStream.Position := 0;

    //send stream object
    FiocpCoderTcpClient.writeObject(lvStream);
  finally
    lvStream.Free;
  end;

end;

procedure TfrmMain.OnDisconnected(pvObject: TObject);
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
var
  lvFileHead, lvResult:TFileHead;
  lvStream, lvFileData:TMemoryStream;
  lvFile:String;
  lvFileStream:TFileStream;
begin
  lvFileData := nil;
  lvStream := TMemoryStream(pvObject);
  if lvStream.Size < SizeOf(TFileHead) then
  begin  // other data
    SetLength(s, lvStream.Size);
    lvStream.Position := 0;
    lvStream.Read(s[1], lvStream.Size);

    uiLogger.logMessage('recv msg from server:' + sLineBreak + '    ' + s);
    uiLogger.logMessage('');
  end else
  begin
    lvStream.Read(Pointer(@lvFileHead)^, SizeOf(TFileHead));
    if lvFileHead.Flag <> FILE_TRANS_FLAG  then
    begin        // other data
      SetLength(s, lvStream.Size);
      lvStream.Position := 0;
      lvStream.Read(s[1], lvStream.Size);

      uiLogger.logMessage('recv msg from server:' + sLineBreak + '    ' + s);
      uiLogger.logMessage('');
    end else
    begin
      try

        if lvFileHead.cmd_result = 3 then
        begin
          raise Exception.Create('request Error param');
        end else if lvFileHead.cmd_result = 2 then
        begin
          raise Exception.Create('unkown server error');
        end else if lvFileHead.cmd_result = 1 then
        begin
          raise Exception.Create('server file not found!');
        end;

        if lvFileHead.cmd = 11 then
        begin    // request file info
          FFileAsyncTrans.ParseRequestFileINfo(@lvFileHead, lvStream);
          uiLogger.logMessage('file info name:%s, size:%d', [FFileAsyncTrans.FileName,
            FFileAsyncTrans.FileSize]);

          FFileAsyncTrans.requestNextFileBlock;
        end else if lvFileHead.cmd = 2 then   // file data
        begin
          Assert(lvFileHead.Size=lvStream.Size - lvStream.Position);
          FFileAsyncTrans.saveFileData(lvStream, lvStream.Size-lvStream.Position);
          if not FFileAsyncTrans.isFileCompleted then
          begin
            uiLogger.logMessage('file data received, current size:%u!', [FFileAsyncTrans.CurrentPostion]);
            FFileAsyncTrans.requestNextFileBlock;
          end else
          begin
            uiLogger.logMessage('file (%s) is down completed, size:%u!', [
              FFileAsyncTrans.FileName, FFileAsyncTrans.FileSize]);
            FFileAsyncTrans.closeAndReset;

          end;
        end;
      except
        on E:Exception do
        begin
          uiLogger.logMessage('file request exception:' + e.Message, []);
        end;
      end;
    end;

  end;
end;

end.
