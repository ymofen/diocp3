unit safeLogger;

interface

uses
  Classes, BaseQueue, SysUtils, SyncObjs;

type
  TLogLevel=(lgvError, lgvWarning, lgvHint, lgvMessage, lgvDebug);
  TLogDataObject = class(TObject)
  private
    FMsg:string;
  end;

  TSafeLogger = class(TObject)
  private
    FOutPutList:TStrings;
    FDataQueue: TBaseQueue;

  public
    constructor Create;
    destructor Destroy; override;
    procedure logMessage(pvMsg: string; pvLevel: TLogLevel = lgvMessage); overload;
    procedure logMessage(pvMsg: string; const args: array of const; pvLevel:
        TLogLevel = lgvMessage); overload;
    procedure setOutputList(pvLines:TStrings);
  end;

implementation


type

  TLogWorker = class(TThread)
  private
    FNotify: TEvent;
  public
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    procedure Execute; override;
  end;



constructor TSafeLogger.Create;
begin
  inherited Create;
  FDataQueue := TBaseQueue.Create();
end;

destructor TSafeLogger.Destroy;
begin
  FreeAndNil(FDataQueue);
  inherited Destroy;
end;

{ TSafeLogger }

procedure TSafeLogger.logMessage(pvMsg: string; pvLevel: TLogLevel =
    lgvMessage);
begin

end;

procedure TSafeLogger.logMessage(pvMsg: string; const args: array of const;
    pvLevel: TLogLevel = lgvMessage);
begin
  logMessage(Format(pvMsg, args));
end;

procedure TSafeLogger.setOutputList(pvLines: TStrings);
begin
  FOutPutList := pvLines;
end;

constructor TLogWorker.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FNotify := TEvent.Create(nil,false,false,'');
end;

destructor TLogWorker.Destroy;
begin
  FNotify.Free;
  inherited Destroy;
end;

procedure TLogWorker.Execute;
begin
  while not self.Terminated do
  begin
    if (FNotify.WaitFor(INFINITE)=wrSignaled) then
    begin
      while  do


    end;
  end;
end;

end.
