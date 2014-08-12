(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v3.0.1(2014-7-16 21:36:30)
 *     + first release
 *
 *   2014-08-12 21:47:34
 *     + add Enable property
 *)
unit iocpTask;

interface

uses
  iocpEngine, SysUtils, BaseQueue, Messages, Windows, Classes, SyncObjs;

const
  WM_REQUEST_TASK = WM_USER + 1;

type
  TIocpTaskRequest = class;
  TIocpTaskMananger = class;

  TOnTaskWorkStrData = procedure(strData: String) of object;
  TOnTaskWorkNoneData = procedure() of object;
  
  TOnTaskWork = procedure(pvTaskRequest: TIocpTaskRequest) of object;
  TOnTaskWorkProc = procedure(pvTaskRequest: TIocpTaskRequest);

  /// rtPostMessage: use in dll project
  TRunInMainThreadType = (rtSync, rtPostMessage);

  TIocpTaskRequest = class(TIocpRequest)
  private
    FOwner:TIocpTaskMananger;
    FMessageEvent: TEvent;
    FStrData:String;
    FOnTaskWork: TOnTaskWork;
    FOnTaskWorkStrData :TOnTaskWorkStrData;
    FOnTaskWorkNoneData :TOnTaskWorkNoneData;

    FRunInMainThread: Boolean;
    FRunInMainThreadType: TRunInMainThreadType;
    FTaskData:Pointer;
    procedure DoCleanUp;
    procedure InnerDoTask;
  protected
    procedure HandleResponse; override;
  public
    constructor Create;
    destructor Destroy; override;
    property TaskData: Pointer read FTaskData;
  end;

  TIocpTaskMananger = class(TObject)
  private
    FActive: Boolean;
    FEnable: Boolean;
    FIocpEngine: TIocpEngine;
    procedure SetActive(const Value: Boolean);
  protected
    FMessageHandle: HWND;
    procedure DoMainThreadWork(var AMsg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   will stop iocpEngine and lose jobs
    /// </summary>
    procedure setWorkerCount(pvCounter:Integer);

    procedure PostATask(pvTaskWork:TOnTaskWorkNoneData; pvRunInMainThread:Boolean =
        False; pvRunType:TRunInMainThreadType = rtSync); overload;

    procedure PostATask(pvTaskWork:TOnTaskWorkStrData; pvStrData:string;
        pvRunInMainThread:Boolean = False; pvRunType:TRunInMainThreadType =
        rtSync); overload;

    procedure PostATask(pvTaskWork:TOnTaskWork;
       pvTaskData:Pointer = nil;
       pvRunInMainThread:Boolean = False;
       pvRunType:TRunInMainThreadType = rtSync);overload;

    procedure PostATask(pvTaskWorkProc: TOnTaskWorkProc; pvTaskData: Pointer = nil;
        pvRunInMainThread: Boolean = False; pvRunType: TRunInMainThreadType =
        rtSync); overload;

       
    property Active: Boolean read FActive write SetActive;

    property Enable: Boolean read FEnable write FEnable;
    
    property IocpEngine: TIocpEngine read FIocpEngine;
  end;

var
  iocpTaskManager: TIocpTaskMananger;

function checkInitializeTaskManager(pvWorkerCount: Integer = 0): Boolean;

implementation

var
  /// iocpRequestPool
  requestPool:TBaseQueue;


function MakeTaskProc(const AProc:TOnTaskWorkProc):TOnTaskWork;
begin
  TMethod(Result).Data:=nil;
  TMethod(Result).Code:=@AProc;
end;
  

function checkInitializeTaskManager(pvWorkerCount: Integer = 0): Boolean;
begin
  if iocpTaskManager = nil then
  begin
    iocpTaskManager := TIocpTaskMananger.Create;
    if pvWorkerCount <> 0 then
    begin
      iocpTaskManager.setWorkerCount(pvWorkerCount);
    end;
    iocpTaskManager.IocpEngine.Name := 'iocpDefaultTaskManager';
    iocpTaskManager.Active := True;
    Result := true;
  end else
  begin
    Result := false;
  end;
end;

constructor TIocpTaskMananger.Create;
begin
  inherited Create;
  FIocpEngine := TIocpEngine.Create();
  FMessageHandle := AllocateHWnd(DoMainThreadWork);
  FActive := false;
end;

destructor TIocpTaskMananger.Destroy;
begin
  FIocpEngine.safeStop;
  FIocpEngine.Free;
  inherited Destroy;
end;

procedure TIocpTaskMananger.DoMainThreadWork(var AMsg: TMessage);
begin
  if AMsg.Msg = WM_REQUEST_TASK then
  begin
    if not FEnable then Exit;
    try
      TIocpTaskRequest(AMsg.WPARAM).InnerDoTask();
    finally
      if AMsg.LPARAM <> 0 then
        TEvent(AMsg.LPARAM).SetEvent;
    end;
  end else
    AMsg.Result := DefWindowProc(FMessageHandle, AMsg.Msg, AMsg.WPARAM, AMsg.LPARAM);
end;

procedure TIocpTaskMananger.PostATask(pvTaskWorkProc: TOnTaskWorkProc;
    pvTaskData: Pointer = nil; pvRunInMainThread: Boolean = False; pvRunType:
    TRunInMainThreadType = rtSync);
begin
  PostATask(MakeTaskProc(pvTaskWorkProc),pvTaskData, pvRunInMainThread, pvRunType);
end;

procedure TIocpTaskMananger.PostATask(pvTaskWork:TOnTaskWorkStrData;
    pvStrData:string; pvRunInMainThread:Boolean = False;
    pvRunType:TRunInMainThreadType = rtSync);
var
  lvRequest:TIocpTaskRequest;
begin
  if not FEnable then Exit;
  lvRequest := TIocpTaskRequest(requestPool.Pop);
  try
    if lvRequest = nil then
    begin
      lvRequest := TIocpTaskRequest.Create;
    end;
    lvRequest.DoCleanUp;

    lvRequest.FOwner := self;
    lvRequest.FOnTaskWorkStrData := pvTaskWork;
    lvRequest.FStrData := pvStrData;
    lvRequest.FRunInMainThread := pvRunInMainThread;
    lvRequest.FRunInMainThreadType := pvRunType;

    /// post request to iocp queue
    if not IocpEngine.IocpCore.postRequest(0, @lvRequest.FOverlapped) then
    begin
      RaiseLastOSError;
    end;
  except
    // if occur exception, push to requestPool.
    if lvRequest <> nil then requestPool.Push(lvRequest);
    raise;
  end;

end;

procedure TIocpTaskMananger.PostATask(pvTaskWork: TOnTaskWork;
  pvTaskData: Pointer; pvRunInMainThread: Boolean;
  pvRunType: TRunInMainThreadType);
var
  lvRequest:TIocpTaskRequest;
begin
  if not FEnable then Exit;
  
  lvRequest := TIocpTaskRequest(requestPool.Pop);
  try
    if lvRequest = nil then
    begin
      lvRequest := TIocpTaskRequest.Create;
    end;
    lvRequest.DoCleanUp;

    lvRequest.FOwner := self;
    lvRequest.FOnTaskWork := pvTaskWork;
    lvRequest.FTaskData := pvTaskData;
    lvRequest.FRunInMainThread := pvRunInMainThread;
    lvRequest.FRunInMainThreadType := pvRunType;
    
    /// post request to iocp queue
    if not IocpEngine.IocpCore.postRequest(0, @lvRequest.FOverlapped) then
    begin
      RaiseLastOSError;
    end;
  except
    // if occur exception, push to requestPool.
    if lvRequest <> nil then requestPool.Push(lvRequest);
    raise;
  end;
end;

procedure TIocpTaskMananger.PostATask(pvTaskWork:TOnTaskWorkNoneData;
    pvRunInMainThread:Boolean = False; pvRunType:TRunInMainThreadType = rtSync);
var
  lvRequest:TIocpTaskRequest;
begin
  if not FEnable then Exit;
  lvRequest := TIocpTaskRequest(requestPool.Pop);
  try
    if lvRequest = nil then
    begin
      lvRequest := TIocpTaskRequest.Create;
    end;
    lvRequest.DoCleanUp;

    lvRequest.FOwner := self;
    lvRequest.FOnTaskWorkNoneData := pvTaskWork;
    lvRequest.FRunInMainThread := pvRunInMainThread;
    lvRequest.FRunInMainThreadType := pvRunType;

    /// post request to iocp queue
    if not IocpEngine.IocpCore.postRequest(0, @lvRequest.FOverlapped) then
    begin
      RaiseLastOSError;
    end;
  except
    // if occur exception, push to requestPool.
    if lvRequest <> nil then requestPool.Push(lvRequest);
    raise;
  end;
end;

procedure TIocpTaskMananger.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      FIocpEngine.start;
      FActive := Value;
    end else
    begin
      FActive := Value;
      FIocpEngine.safeStop;
    end;
  end;
end;

procedure TIocpTaskMananger.setWorkerCount(pvCounter: Integer);
begin
  if Active then
  begin
    SetActive(False);
    FIocpEngine.setWorkerCount(pvCounter);
    SetActive(True);
  end else
  begin
    FIocpEngine.setWorkerCount(pvCounter);
  end;
end;

constructor TIocpTaskRequest.Create;
begin
  inherited Create;
  FMessageEvent := TEvent.Create(nil, true, False, '');
end;

destructor TIocpTaskRequest.Destroy;
begin
  FreeAndNil(FMessageEvent);
  inherited Destroy;
end;

{ TIocpTaskRequest }

procedure TIocpTaskRequest.DoCleanUp;
begin
  FOnTaskWork := nil;
  FRunInMainThreadType := rtSync;
  FMessageEvent.ResetEvent;
  FOwner := nil;
end;

procedure TIocpTaskRequest.HandleResponse;
begin
  if FOwner.Active then
  begin
    if FRunInMainThread then
    begin
      case FRunInMainThreadType of
        rtSync: iocpWorker.Synchronize(iocpWorker, InnerDoTask);
        rtPostMessage:
          begin
            FMessageEvent.ResetEvent;
            if PostMessage(FOwner.FMessageHandle, WM_REQUEST_TASK, WPARAM(Self), LPARAM(FMessageEvent)) then
            begin
              FMessageEvent.WaitFor(INFINITE);
            end else
            begin
              // log exception
            end;
          end;
        else
          begin
            //log unkown type
          end;
      end;
    end else
    begin
      InnerDoTask;
    end;
  end;
  requestPool.Push(Self);
end;

procedure TIocpTaskRequest.InnerDoTask;
begin
  if Assigned(FOnTaskWork) then
  begin
    FOnTaskWork(Self);
  end else if Assigned(FOnTaskWorkStrData) then
  begin
    FOnTaskWorkStrData(FStrData);
  end else if Assigned(FOnTaskWorkNoneData) then
  begin
    FOnTaskWorkNoneData();
  end;
end;



initialization
  requestPool := TBaseQueue.Create;
  requestPool.Name := 'taskRequestPool';
  checkInitializeTaskManager(2);


finalization
  if iocpTaskManager <> nil then
  begin
    iocpTaskManager.Free;
  end;
  requestPool.freeDataObject;
  requestPool.Free;



end.

