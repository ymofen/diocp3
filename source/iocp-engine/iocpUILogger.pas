unit iocpUILogger;

interface

uses
  iocpTask, Classes, SysUtils;

type
  TIocpUILogger = class(TIocpTaskMananger)
  private
    FLogs: TStrings;
    procedure onLogMessage(pvMsg:String);
  public
    procedure logMessage(pvMsg:string);overload;

    procedure logMessage(pvMsg:string; const args: array of const);overload;
    procedure setLogLines(pvLines:TStrings);
  end;


var
  uiLogger:TIocpUILogger;

implementation

{ TIocpUILogger }

procedure TIocpUILogger.logMessage(pvMsg: string);
begin
  PostATask(onLogMessage, pvMsg, True, rtPostMessage);
end;

procedure TIocpUILogger.logMessage(pvMsg: string; const args: array of const);
begin
  logMessage(Format(pvMsg, args));
end;

procedure TIocpUILogger.onLogMessage(pvMsg: String);
begin
  if FLogs <> nil then
  begin
    FLogs.Add(pvMsg);
  end;
end;

procedure TIocpUILogger.setLogLines(pvLines: TStrings);
begin
  FLogs := pvLines;
end;


initialization
  uiLogger := TIocpUILogger.Create;
  uiLogger.setWorkerCount(1);
  uiLogger.Active := true;

finalization
  uiLogger.Free;






end.
