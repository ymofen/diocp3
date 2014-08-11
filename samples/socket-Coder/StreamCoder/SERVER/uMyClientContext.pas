unit uMyClientContext;

interface

uses
  uIOCPCentre, iocpLogger, FileTransProtocol, SysUtils, Classes, Windows, Math;


type
  TMyClientContext = class(TIOCPCoderClientContext)
  private
    FFileStream:TFileStream;
    FFileName:String;
    procedure checkFreeFileStream;
    procedure checkCreateFileStream(pvFileName: string);
  protected
    procedure OnDiscounnected; override;

    procedure OnConnected; override;
  protected
    /// <summary>
    ///   数据处理
    /// </summary>
    /// <param name="pvObject"> (TObject) </param>
    procedure dataReceived(const pvObject: TObject); override;
  end;

implementation

procedure TMyClientContext.checkCreateFileStream(pvFileName: string);
begin
  if (FFileStream = nil) or (pvFileName <> FFileName) then
  begin
    checkFreeFileStream;
    FFileStream := TFileStream.Create(pvFileName, fmOpenRead and fmShareDenyWrite);
    FFileName := pvFileName;
  end;
end;

procedure TMyClientContext.checkFreeFileStream;
begin
  if FFileStream <> nil then
  begin
    FFileStream.Free;
    FFileStream := nil;
  end;
end;

procedure TMyClientContext.dataReceived(const pvObject: TObject);
var
  lvFileHead, lvResult:TFileHead;
  lvStream, lvFileData:TMemoryStream;
  lvFile:String;
begin
  lvFileData := nil;
  lvStream := TMemoryStream(pvObject);
  if lvStream.Size < SizeOf(TFileHead) then
  begin  // other data
    writeObject(pvObject);
  end else
  begin
    lvStream.Read(lvFileHead, SizeOf(TFileHead));
    if lvFileHead.Flag <> FILE_TRANS_FLAG  then
    begin        // other data
      writeObject(pvObject);
    end else
    begin
      try
        ZeroMemory(@lvResult, SizeOf(TFilehead));
        lvResult.Flag := FILE_TRANS_FLAG;

        if lvFileHead.cmd = 10 then
        begin    // request file info
          lvResult.cmd := 11;  //response
          lvResult.FileName := lvFileHead.FileName;
          uiLogger.logMessage('request file(%s) info', [lvFileHead.FileName]);

          lvFile := ExtractFilePath(ParamStr(0)) + 'files\' + lvFileHead.FileName;


          if not FileExists(lvFile) then
          begin
            lvResult.cmd_result := 1;  // file not found
          end else
          begin
            checkCreateFileStream(lvFile);

            lvResult.Size := FFileStream.Size;
          end;

        end else if lvFileHead.cmd = 1 then
        begin                 // down file
          lvResult.cmd := 2;  //  response

          lvFile := ExtractFilePath(ParamStr(0)) + 'files\' + lvFileHead.FileName;

          if not FileExists(lvFile) then
          begin
            lvResult.cmd_result := 1;  // file not found
          end else
          begin
            checkCreateFileStream(lvFile);

            if FFileStream.Size >= lvFileHead.Position then
            begin
              FFileStream.Position := lvFileHead.Position;
              if FFileStream.Size = lvFileHead.Position then
              begin
                lvResult.Size := 0;
              end else
              begin 
                lvFileData := TMemoryStream.Create;
                lvResult.Size := Min(FFileStream.Size - FFileStream.Position, lvFileHead.Size);

                // file data size
                lvFileData.CopyFrom(FFileStream, lvResult.Size);
              end;

              
              if FFileStream.Position = FFileStream.Size then
              begin      // end
                FFileStream.Free;
                FFileStream := nil;
              end;


            end  else
              // err param
              lvResult.cmd_result := 3;

          end;
        end;
        // return response
        lvStream.Clear;
        lvStream.WriteBuffer(lvResult, SizeOf(lvResult));
        if lvFileData <> nil then
        begin
          // return fileStream data
          lvFileData.Position := 0;
          lvStream.CopyFrom(lvFileData,lvFileData.Size);
        end;
        lvStream.Position := 0;
      except
        on E:Exception do
        begin
          uiLogger.logMessage('file trans exception:' + e.Message, []);
          lvStream.Clear;
          lvResult.cmd_result := 2;
          lvStream.WriteBuffer(lvResult, SizeOf(lvResult));
        end;
      end;
      writeObject(lvStream);


      // free file stream
      if lvFileData <> nil then
      begin
        lvFileData.Free;
      end;


    end;
  end;
end;

procedure TMyClientContext.OnConnected;
begin

end;

procedure TMyClientContext.OnDiscounnected;
begin
  checkFreeFileStream;
end;

end.
