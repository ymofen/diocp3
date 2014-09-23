unit FileLogger;

interface

uses
  SysUtils, SyncObjs, safeLogger;

type
  TFileLogger = class(TObject)
  private
    FSafeLogger:TSafeLogger;
    FFilePreFix:String;
    FAddThreadINfo:Boolean;
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
  FSafeLogger := TSafeLogger.Create;
  FSafeLogger.setAppender(TLogFileAppender.Create(True));
end;

destructor TFileLogger.Destroy;
begin
  FSafeLogger.Free;
  inherited Destroy;
end;

class function TFileLogger.instance: TFileLogger;
begin
  Result := __instance;
end;

procedure TFileLogger.logDebugMessage(pvMsg:String);
begin
  FSafeLogger.logMessage(pvMsg, FFilePreFix + 'DEBUG_', lgvDebug);
end;

procedure TFileLogger.logErrMessage(pvMsg:String);
begin
  FSafeLogger.logMessage(pvMsg, FFilePreFix + 'ERR_', lgvError);
end;

procedure TFileLogger.logMessage(pvMsg: string; pvLogFilePre: string = '');
var
  lvPreFix:String;
begin
  if pvLogFilePre = '' then
  begin
    lvPreFix := FFilePreFix;
  end else
  begin
    lvPreFix := pvLogFilePre;
  end;
  FSafeLogger.logMessage(pvMsg, lvPreFix, lgvMessage);

end;


procedure TFileLogger.logMessageWithoutLocker(pvMsg: string; pvLogFilePre:
    string = '');
var
  lvPreFix:String;
begin
  if pvLogFilePre = '' then
  begin
    lvPreFix := FFilePreFix;
  end else
  begin
    lvPreFix := pvLogFilePre;
  end;
  FSafeLogger.logMessage(pvMsg, lvPreFix, lgvMessage);
end;

procedure TFileLogger.setAddThreadINfo(pvBoolean:Boolean);
begin
  TLogFileAppender(FSafeLogger.Appender).AddThreadINfo := pvBoolean;
end;

procedure TFileLogger.setFilePre(pvPre:String);
begin
  FFilePreFix := pvPre;
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
