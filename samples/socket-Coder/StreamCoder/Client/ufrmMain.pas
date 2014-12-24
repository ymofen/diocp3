unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpCoderClient,
  iocpLogger, uDIOCPDxStreamCoder, IocpFileASyncTrans, iocpTask, iocpBaseSocket;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    btnGetFile: TButton;
    edtFileID: TEdit;
    mmoData: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure btnGetFileClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
  private
    { Private declarations }
    FIocpClient:TIocpCoderRemoteContext;
    FiocpCoderTcpClient:TIocpCoderClient;

    //
    FFileAsyncTrans: TIocpFileASyncTrans;

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

uses
  FileTransProtocol;





{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  uiLogger.setLogLines(mmoRecvMessage.Lines);

  FFileAsyncTrans := TIocpFileASyncTrans.Create;
  FiocpCoderTcpClient := TIocpCoderClient.Create(Self);
  FIocpClient :=TIocpCoderRemoteContext(FiocpCoderTcpClient.Add);

  FIocpClient.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FIocpClient.OnDataObjectReceived := OnRecvObject;
  FiocpCoderTcpClient.OnContextDisconnected := OnDisconnected;
  FFileAsyncTrans.IocpClient := FIocpClient;


end;

destructor TfrmMain.Destroy;
begin
  FiocpCoderTcpClient.DisconnectAll;
  FiocpCoderTcpClient.Free;
  FFileAsyncTrans.Free;
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

procedure TfrmMain.btnGetFileClick(Sender: TObject);
begin
  if not FIocpClient.Active then
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
