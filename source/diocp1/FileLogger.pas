unit FileLogger;

interface

uses
  SysUtils, SyncObjs;

type
  TFileLogger = class(TObject)
  private
    FAddThreadINfo:Boolean;
    FPre:string;
    FBasePath: string;
    FLogFile: TextFile;
    FLocker: TCriticalSection;
    function openLogFile(pvPre: String = ''): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure logMessageWithoutLocker(pvMsg: string; pvLogFilePre: string = '');
  public
    /// <summary>
    ///   设置文件名前缀
    ///   例如"ERR_"
    /// </summary>
    /// <param name="pvPre"> (String) </param>
    procedure setFilePre(pvPre:String);

    /// <summary>
    ///   设置添加是否在日志中添加线程信息
    ///   默认为false
    /// </summary>
    /// <param name="pvBoolean"> (Boolean) </param>
    procedure setAddThreadINfo(pvBoolean:Boolean);

    procedure checkReady();

    procedure logMessage(pvMsg: string; pvLogFilePre: string = '');

    procedure logErrMessage(pvMsg:String);

    procedure logDebugMessage(pvMsg:String);

    class function instance: TFileLogger;
  end;

implementation

uses
  Windows;

var
  __instance:TFileLogger;
  

constructor TFileLogger.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
  checkReady;
end;

destructor TFileLogger.Destroy;
begin
  FLocker.Free;
  inherited Destroy;
end;

{ TFileLogger }

procedure TFileLogger.checkReady;
begin
  FBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
  if not DirectoryExists(FBasePath) then CreateDir(FBasePath);
end;

class function TFileLogger.instance: TFileLogger;
begin
  Result := __instance;
end;

procedure TFileLogger.logDebugMessage(pvMsg:String);
begin
  FLocker.Enter;
  try
    logMessageWithoutLocker(pvMsg, 'DEBUG_');
  finally
    FLocker.Leave;
  end;
end;

procedure TFileLogger.logErrMessage(pvMsg:String);
begin
  FLocker.Enter;
  try
    logMessageWithoutLocker(pvMsg, 'ERR_');
  finally
    FLocker.Leave;
  end;
end;

procedure TFileLogger.logMessage(pvMsg: string; pvLogFilePre: string = '');
begin
  FLocker.Enter;
  try
    logMessageWithoutLocker(pvMsg, pvLogFilePre);
  finally
    FLocker.Leave;
  end;
end;


procedure TFileLogger.logMessageWithoutLocker(pvMsg: string; pvLogFilePre:
    string = '');
var
  lvPre:String;
  lvFile:String;
begin
  lvFile := pvLogFilePre;//getThreadPreFile + '_' + pvLogFilePre;
  if OpenLogFile(lvFile) then
  try
    lvPre := FormatDateTime('hh:nn:ss:zzz', Now) + ' ';
    if FAddThreadINfo then
    begin
      lvPre := lvPre + Format('ProcessID: %d  ThreadID: %d ',
        [
         GetCurrentProcessID(),
         GetCurrentThreadID()
        ]);
    end;
    writeln(FLogFile,lvPre + pvMsg);
    flush(FLogFile);
  finally
    CloseFile(FLogFile);
  end;
end;

function TFileLogger.openLogFile(pvPre: String = ''): Boolean;
var
  lvFileName:String;
  lvPre :String;
begin
  if pvPre <> '' then
  begin
    lvPre := pvPre;
  end else
  begin
    lvPre := FPre;
  end;

  lvFileName :=FBasePath + '\' + lvPre + FormatDateTime('yyyymmddhh', Now()) + '.txt';
  try
    AssignFile(FLogFile, lvFileName);
    if (FileExists(lvFileName)) then
      append(FLogFile)
    else
      rewrite(FLogFile);

    Result := true;
  except
    Result := false;
  end;
end;

procedure TFileLogger.setAddThreadINfo(pvBoolean:Boolean);
begin
  FAddThreadINfo := pvBoolean;
end;

procedure TFileLogger.setFilePre(pvPre:String);
begin
  FPre := pvPre;
end;

initialization
  __instance := TFileLogger.Create;

finalization
  if __instance <> nil then
  begin
    __instance.Free;
    __instance := nil;
  end;

end.
