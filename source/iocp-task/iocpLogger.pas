unit iocpLogger;

interface

uses
  iocpTask, Classes, SysUtils;

type
  TIocpLogger = class(TObject)
  private
    FLogs: TStrings;
    procedure onLogMessage(pvMsg:String);
  public
    procedure logMessage(pvMsg:string);overload;

    procedure logMessage(pvMsg:string; const args: array of const);overload;
    procedure setLogLines(pvLines:TStrings);
  end;


var
  uiLogger:TIocpLogger;

implementation

{ TIocpLogger }

procedure TIocpLogger.logMessage(pvMsg: string);
begin
  iocpTaskManager.PostATask(onLogMessage, pvMsg, True, rtPostMessage);
end;

procedure TIocpLogger.logMessage(pvMsg: string; const args: array of const);
begin
  logMessage(Format(pvMsg, args));
end;

procedure TIocpLogger.onLogMessage(pvMsg: String);
begin
  if FLogs <> nil then
  begin
    FLogs.Add(pvMsg);
  end;
end;

procedure TIocpLogger.setLogLines(pvLines: TStrings);
begin
  FLogs := pvLines;
end;


initialization
  uiLogger := TIocpLogger.Create;

finalization
  uiLogger.Free;






end.
