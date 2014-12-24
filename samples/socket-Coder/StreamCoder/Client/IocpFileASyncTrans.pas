unit IocpFileASyncTrans;

interface

uses
  iocpCoderClient, FileTransProtocol, Classes, SysUtils;

type
  TIocpFileASyncTrans = class(TObject)
  private
    FBlockSize: Integer;
    FRequestHead:TFileHead;
    FCMDFileStream:TMemoryStream;

    FLocalFileName:String;
    FFileName: string;
    FFileSize: Int64;
    FFileStream: TFileStream;

    FIocpTcpCoderTcpClient: TIocpCoderRemoteContext;
    FRootPath: String;

    procedure checkCreateFileStream();
    function GetCurrentPostion: Int64;
    procedure writeRequestHead;

    procedure sendCMD();



  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   parse request file info
    ///     size...
    /// </summary>
    procedure ParseRequestFileINfo(pvFileHead: PFileHead; pvStream: TStream);

    procedure saveFileData(pvStream:TStream; pvLen:Cardinal);

    function isFileCompleted:Boolean;

    /// <summary>
    ///   request file data
    /// </summary>
    procedure requestNextFileBlock();

    procedure requestFileINfo(pvRemoteFileName: string);

    procedure closeAndReset;


    property BlockSize: Integer read FBlockSize write FBlockSize;
    property CurrentPostion: Int64 read GetCurrentPostion;

    property FileName: string read FFileName;
    property FileSize: Int64 read FFileSize;








    property IocpClient: TIocpCoderRemoteContext read FIocpTcpCoderTcpClient write
        FIocpTcpCoderTcpClient;
    property RootPath: String read FRootPath write FRootPath;
  end;

implementation

procedure TIocpFileASyncTrans.closeAndReset;
begin
  FFileStream.Free;
  FFileStream := nil;
  FCMDFileStream.Clear;
  FFileName := '';

end;

constructor TIocpFileASyncTrans.Create;
begin
  inherited Create;
  //
  FBlockSize := 1024 * 50;

  FCMDFileStream := TMemoryStream.Create;

  FRootPath := ExtractFilePath(ParamStr(0)) + 'downFiles\';
end;

destructor TIocpFileASyncTrans.Destroy;
begin
  FFileStream.Free;
  inherited Destroy;
end;

function TIocpFileASyncTrans.isFileCompleted: Boolean;
begin
  Result := FFileSize = FFileStream.Size;
end;

procedure TIocpFileASyncTrans.checkCreateFileStream;
begin
  if FFileStream = nil then
  begin
    FLocalFileName := FRootPath + FFileName;
    ForceDirectories(ExtractFilePath(FLocalFileName));
    if FileExists(FLocalFileName) then
    begin
      FFileStream := TFileStream.Create(FLocalFileName, fmOpenWrite or fmShareDenyWrite);
    end else
    begin
      FFileStream := TFileStream.Create(FLocalFileName, fmCreate or fmShareDenyWrite);
    end;
    FFileStream.Position := FFileStream.Size;
  end;
end;

function TIocpFileASyncTrans.GetCurrentPostion: Int64;
begin
  Result := FFileStream.Position;
end;

procedure TIocpFileASyncTrans.ParseRequestFileINfo(pvFileHead: PFileHead;
    pvStream: TStream);
begin
  if FFileStream <> nil then
  begin
    FFileStream.Free;
    FFileStream := nil;
  end;
  FFileSize := pvFileHead.Size;
  FFileName := pvFileHead.FileName;
end;

procedure TIocpFileASyncTrans.requestFileINfo(pvRemoteFileName: string);
begin
  FRequestHead.Flag := FILE_TRANS_FLAG;
  FRequestHead.cmd := 10;   // reqeust file info
  FRequestHead.FileName := pvRemoteFileName;

  FCMDFileStream.Clear;

  writeRequestHead;

  sendCMD;
end;

procedure TIocpFileASyncTrans.requestNextFileBlock;
begin
  checkCreateFileStream;

  FRequestHead.Flag := FILE_TRANS_FLAG;
  FRequestHead.cmd := 1;                   // reqeust file size
  FRequestHead.FileName := FFileName;       // request file
  FRequestHead.Position := FFileStream.Position; // start position
  FRequestHead.Size := FBlockSize;    // request size

  FCMDFileStream.Clear;

  writeRequestHead;

  sendCMD;
end;

procedure TIocpFileASyncTrans.saveFileData(pvStream: TStream; pvLen: Cardinal);
begin
  if FFileStream = nil then
  begin
    raise Exception.Create('File Stream is not valid!');
  end;
  if pvLen > 0 then
  begin
    FFileStream.CopyFrom(pvStream, pvLen);
  end else
  begin
    raise Exception.Create('no data need to append!');
  end;
end;

procedure TIocpFileASyncTrans.sendCMD;
begin
  FCMDFileStream.Position := 0;
  FIocpTcpCoderTcpClient.writeObject(FCMDFileStream);
end;

procedure TIocpFileASyncTrans.writeRequestHead;
begin
  FCMDFileStream.Write(FRequestHead, SizeOf(FRequestHead));
end;



end.
