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
    btnGetFile: TButton;
    edtFileID: TEdit;
    procedure btnConnectClick(Sender: TObject);
    procedure btnGetFileClick(Sender: TObject);
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
  FileTransProtocol;





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

procedure TfrmMain.btnGetFileClick(Sender: TObject);
var
  lvStream:TMemoryStream;
  lvFileHead:TFileHead;
begin
  if not  FiocpCoderTcpClient.isActive then
  begin
    uiLogger.logMessage('please do connect');
    exit;
  end;

  ZeroMemory(@lvFileHead, SizeOf(lvFileHead));

  lvFileHead.Flag := FILE_TRANS_FLAG;
  lvFileHead.cmd := 10;   // file info
  lvFileHead.FileName := edtFileID.Text;
  lvStream := TMemoryStream.Create;
  lvStream.Write(lvFileHead, SizeOf(lvFileHead));
  FiocpCoderTcpClient.writeObject(lvStream);
  lvStream.Free; 
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
        if lvFileHead.cmd = 11 then
        begin    // request file info
          uiLogger.logMessage('file info name:%s, size:%d', [lvFileHead.FileName,
            lvFileHead.Size]);


        end else if lvFileHead.cmd = 2 then
        begin                 // file data
//          lvFile := ExtractFilePath(ParamStr(0)) + 'files\' + lvFileHead.FileName;
//          if not FileExists(lvFile) then
//          begin
//            lvResult.cmd_result := 1;  // file not found
//          end else
//          begin
//            lvFileStream := TFileStream.Create(lvFile, fmOpenRead);
//            try
//              if lvFileStream.Position > lvFileHead.Position then
//              begin
//                lvFileStream.Position := lvFileHead.Position;
//                lvFileData := TMemoryStream.Create;
//                lvFileData.CopyFrom(lvFileData, lvFileHead.Size);
//              end  else
//                // err param
//                lvResult.cmd_result := 3;
//            finally
//              lvFileStream.Free;
//            end;
//          end;
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
