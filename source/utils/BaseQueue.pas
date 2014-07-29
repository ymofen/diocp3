unit BaseQueue;

interface

uses
  SyncObjs;

{$DEFINE __debug}

type
  PQueueData = ^TQueueData;
  TQueueData = record
    Data: Pointer;
    Next: PQueueData;
  end;

  TBaseQueue = class(TObject)
  private
    
    FLocker: TCriticalSection;
    FCount: Integer;
    FHead: PQueueData;
    FName: String;
    FTail: PQueueData;

    {$IFDEF __debug}
    FPopCounter:Integer;
    FPushCounter:Integer;
    {$ENDIF}

    /// <summary>
    ///   清空所有数据
    /// </summary>
    procedure clear;
    function innerPop: PQueueData;
    procedure innerPush(AData: PQueueData);
  public
    constructor Create;
    destructor Destroy; override;
    function IsEmpty: Boolean;

    function size:Integer;

    function Pop: Pointer;
    procedure Push(AData: Pointer);

    /// <summary>
    ///  invoke Only Data Pointer is TObject
    /// </summary>
    procedure FreeDataObject;

    property Name: String read FName write FName;




  end;



implementation


type
  /// <summary>
  ///  reference TJobPool in qdac3 
  /// </summary>
  TQueueDataPool = class
  protected
    FFirst: PQueueData;
    FCount: Integer;
    FSize: Integer;
    FLocker: TCriticalSection;

    {$IFDEF __debug}
    FPopCounter:Integer;
    FPushCounter:Integer;
    {$ENDIF}
  public
    constructor Create(AMaxSize: Integer = 2048); overload;
    destructor Destroy; override;
    procedure Push(pvQueueData: PQueueData);
    function Pop: PQueueData;
    property Count: Integer read FCount;
    property Size: Integer read FSize write FSize;
  end;

var
  // data pool of PQueueData
  queueDataPool :TQueueDataPool;

constructor TBaseQueue.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
  FHead := queueDataPool.Pop;
  FHead.next := nil;
  FTail := FHead;
  FCount := 0;
  FName := 'BaseQueue';
end;

destructor TBaseQueue.Destroy;
begin
  {$IFDEF __debug}
    Assert(FPopCounter = FPushCounter, ('[' + FName + ']PopCounter <> PushCounter'));
  {$ENDIF}

  Clear;
  FLocker.Free;
  queueDataPool.Push(FHead);
  inherited Destroy;
end;

{ TBaseQueue }

procedure TBaseQueue.clear;
var
  ANext: PQueueData;
begin
  FLocker.Enter;
  try
    while FHead.Next <> nil do
    begin
      ANext := FHead.Next;     
      
      queueDataPool.Push(FHead);
      FHead := ANext;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TBaseQueue.freeDataObject;
var
  lvData:Pointer;
begin
  while True do
  begin
    lvData := self.Pop;
    if lvData = nil then Break;
    TObject(lvData).Free;      
  end;
end;

function TBaseQueue.IsEmpty: Boolean;
begin
  Result := (FHead.next = nil);
end;

function TBaseQueue.Pop: Pointer;
var
  lvTemp:PQueueData;
begin
  Result := nil;
  lvTemp := innerPop;
  if lvTemp <> nil then
  begin
    Result := lvTemp.Data;
    queueDataPool.Push(lvTemp);
  end;
end;

procedure TBaseQueue.Push(AData: Pointer);
var
  lvTemp:PQueueData;
begin
  lvTemp := queueDataPool.Pop;
  lvTemp.Data := AData;
  innerPush(lvTemp);
end;

function TBaseQueue.size: Integer;
begin
  Result := FCount;
end;

function TBaseQueue.innerPop: PQueueData;
begin
  ///为了方便 队列中始终保留一个FHead数据块
  ///  也就是说FHead指向的下一个数据块才是第一个数据块
  FLocker.Enter;
  try
    Result := FHead.Next;
    if Result <> nil then
    begin
      FHead.Next := Result.Next;
      if Result = FTail then
        FTail := FHead;

      Dec(FCount);

    {$IFDEF __debug}
      Inc(FPopCounter);
    {$ENDIF}
      
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TBaseQueue.innerPush(AData: PQueueData);
begin
  AData.Next := nil;
  FLocker.Enter;
  try
    FTail.Next := AData;
    FTail := AData;
    Inc(FCount);

    {$IFDEF __debug}
      Inc(FPushCounter);
    {$ENDIF}

  finally
    FLocker.Leave;
  end;
end;

{ TQueueDataPool }

constructor TQueueDataPool.Create(AMaxSize: Integer = 2048);
begin
  inherited Create;
  FSize := AMaxSize;
  FLocker := TCriticalSection.Create;
end;

destructor TQueueDataPool.Destroy;
var
  lvData: PQueueData;
begin
  {$IFDEF __debug}
    Assert(FPopCounter = FPushCounter, ('PopCounter <> PushCounter'));
  {$ENDIF}

  FLocker.Enter;
  while FFirst <> nil do
  begin
    lvData := FFirst.Next;
    Dispose(FFirst);
    FFirst := lvData;
  end;
  FLocker.Free;
  inherited;
end;

function TQueueDataPool.Pop: PQueueData;
begin
  FLocker.Enter;
  Result := FFirst;
  if Result <> nil then
  begin
    FFirst := Result.Next;
    Dec(FCount);
  end;
  {$IFDEF __debug}
    Inc(FPopCounter);
  {$ENDIF}
  FLocker.Leave;

  if Result = nil then
    GetMem(Result, SizeOf(TQueueData));
  Result.Data := nil;
  Result.Next := nil;
end;

procedure TQueueDataPool.Push(pvQueueData: PQueueData);
var
  ADoFree: Boolean;
begin
  FLocker.Enter;
  ADoFree := (FCount = FSize);
  if not ADoFree then
  begin
    pvQueueData.Next := FFirst;
    FFirst := pvQueueData;
    Inc(FCount);
  end;
  {$IFDEF __debug}
    Inc(FPushCounter);
  {$ENDIF}
  FLocker.Leave;
  
  if ADoFree then
  begin
    FreeMem(pvQueueData);
  end;
end; 


initialization
  queueDataPool := TQueueDataPool.Create(10240);

finalization
  queueDataPool.Free;


end.
