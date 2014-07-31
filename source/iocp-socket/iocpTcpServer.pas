(*
 *	 Unit owner: d10.ÃÏµÿœ“
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v3.0.1(2014-7-16 21:36:30)
 *     + first release
 *
 *   thanks qsl's suggestion
 *)
 
unit iocpTcpServer;

interface

uses
  Classes, iocpSocketUtils, iocpEngine, iocpProtocol,
  iocpWinsock2,
  iocpRawSocket, SyncObjs, Windows, SysUtils,
  BaseQueue, iocpLocker, FileLogger, iocpUILogger;

const
  buf_size = 1024 * 50;

  MAX_SEND_BLOCK_SIZE = 1024 * 50;

type
  TIocpTcpServer = class;
  TIocpAcceptorMgr = class;
  TIocpClientContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;

  TIocpClientContextClass = class of TIocpClientContext;

  TOnClientContextError = procedure(pvClientContext: TIocpClientContext; errCode:
      Integer) of object;

  TOnDataReceived = procedure(pvClientContext:TIocpClientContext;
      buf:Pointer; len:cardinal; errCode:Integer) of object;

  TOnClientContextConnect = procedure(pvClientContext:TIocpClientContext; errCode:Integer) of object;

  /// <summary>
  ///   on post request is completed
  /// </summary>
  TOnDataRequestCompleted = procedure(pvClientContext:TIocpClientContext;
      pvRequest:TIocpRequest) of object;

  /// <summary>
  ///   client object
  /// </summary>
  TIocpClientContext = class(TObject)
  private
    FDebugINfo: string;
    procedure SetDebugINfo(const Value: string);
  public
    FAlive:Boolean;

    // link
    FPre:TIocpClientContext;
    FNext:TIocpClientContext;

    /// <summary>
    ///  sending flag
    /// </summary>
    FSending: Boolean;

    FActive: Boolean;

    FOwner: TIocpTcpServer;

    FRecvRequest:TIocpRecvRequest;

    /// sendRequest link
    FSendRequestLink: TIocpRequestSingleLink;

    FRawSocket: TRawSocket;

    FRemoteAddr: String;
    FRemotePort: Integer;

    procedure checkRequestObject;

    /// <summary>
    ///   called by recvRequest response
    /// </summary>
    procedure DoReceiveData;

    /// <summary>
    ///   called by sendRequest response
    /// </summary>
    procedure DoSendRequestCompleted(pvRequest: TIocpSendRequest);

    /// <summary>
    ///   request recv data
    /// </summary>
    procedure PostWSARecvRequest();


    /// <summary>
    ///   post next sendRequest
    /// </summary>
    procedure checkNextSendRequest();

    /// <example>
    ///  sendRequest to pool
    /// </example>
    procedure checkReleaseRes;

  public
    procedure DoConnected;

    procedure DoCleanUp;virtual;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DoDisconnect;

    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf:Pointer; len:Cardinal):Boolean;

    property Active: Boolean read FActive;

    property DebugINfo: string read FDebugINfo write SetDebugINfo;

    property Owner: TIocpTcpServer read FOwner;
    property RawSocket: TRawSocket read FRawSocket;

    property RemoteAddr: String read FRemoteAddr;

    property RemotePort: Integer read FRemotePort;


  end;

  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FInnerBuffer: iocpWinsock2.TWsaBuf;
    FPostBuffer: iocpWinsock2.TWsaBuf;
    FRecvdFlag: Cardinal;
    FOwner: TIocpTcpServer;
    FClientContext:TIocpClientContext;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;

    /// <summary>
    ///   post recv request to iocp queue
    /// </summary>
    function PostRequest: Boolean; overload;

    /// <summary>
    ///   
    /// </summary>
    function PostRequest(pvBuffer:PAnsiChar; len:Cardinal): Boolean; overload;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   WSASend io request
  /// </summary>
  TIocpSendRequest = class(TIocpRequest)
  private
    FAlive: Boolean;

    FBytesSize:Cardinal;

    FSendCounter:Integer;

    FReponseCounter:Integer;

    // send buf record
    FWSABuf:TWsaBuf;


    FBuf:Pointer;
    FPosition:Cardinal;
    FLen:Cardinal;

    FOwner: TIocpTcpServer;

    FClientContext:TIocpClientContext;

    FNext:TIocpSendRequest;
    FOnDataRequestCompleted: TOnDataRequestCompleted;
    /// <summary>
    ///   start to post
    /// </summary>
    procedure start;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;


    procedure DoCleanUp;virtual;

    /// <summary>
    ///   post send buffer to iocp queue
    /// </summary>
    function InnerPostRequest(buf: Pointer; len: Cardinal): Boolean;

    /// <summary>
    ///   post send a block
    /// </summary>
    procedure checkSendNextBlock();
  public
    constructor Create;

    destructor Destroy; override;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure setBuffer(buf:Pointer; len:Cardinal);

    /// <summary>
    ///   on entire buf send completed
    /// </summary>
    property OnDataRequestCompleted: TOnDataRequestCompleted read
        FOnDataRequestCompleted write FOnDataRequestCompleted;
  end;


  /// <summary>
  ///   acceptEx request
  /// </summary>
  TIocpAcceptExRequest = class(TIocpRequest)
  private
    /// <summary>
    ///   acceptEx lpOutBuffer[in]
    ///     A pointer to a buffer that receives the first block of data sent on a new connection,
    ///       the local address of the server, and the remote address of the client.
    ///       The receive data is written to the first part of the buffer starting at offset zero,
    ///       while the addresses are written to the latter part of the buffer.
    ///       This parameter must be specified.
    /// </summary>
    FAcceptBuffer: array [0.. (SizeOf(TSockAddrIn) + 16) * 2 - 1] of byte;

    FOwner: TIocpTcpServer;
    FAcceptorMgr:TIocpAcceptorMgr;

    FClientContext:TIocpClientContext;
    /// <summary>
    ///   get socket peer info on acceptEx reponse
    /// </summary>
    procedure getPeerINfo;
  protected
    function PostRequest: Boolean;

  protected
    procedure HandleResponse; override;
    
  public
    constructor Create(AOwner: TIocpTcpServer);
  end;

  /// <summary>
  ///   manager acceptEx request
  /// </summary>
  TIocpAcceptorMgr = class(TObject)
  private
    FOwner: TIocpTcpServer;
    FList:TList;
    FLocker: TIocpLocker;
    FMaxRequest:Integer;
    FMinRequest:Integer;

    procedure notifyForDestory;
  protected
  public
    constructor Create(AOwner: TIocpTcpServer);
    destructor Destroy; override;

    procedure releaseRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure removeRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure checkPostRequest;
  end;

  /// <summary>
  ///   iocp data monitor
  /// </summary>
  TIocpDataMonitor = class(TObject)
  private
    FSentSize:Int64;
    FRecvSize:Int64;
    FPostWSASendSize: Int64;

    FPostWSASendCounter:Integer;
    FResponseWSASendCounter:Integer;

    FPostWSARecvCounter:Integer;
    FResponseWSARecvCounter:Integer;

    FPostWSAAcceptExCounter:Integer;
    FResponseWSAAcceptExCounter:Integer;

    FLocker: TCriticalSection;

    procedure incSentSize(pvSize:Cardinal);
    procedure incPostWSASendSize(pvSize:Cardinal);
    procedure incRecvdSize(pvSize:Cardinal);

    procedure incPostWSASendCounter();
    procedure incResponseWSASendCounter;

    procedure incPostWSARecvCounter();
    procedure incResponseWSARecvCounter;
  public
    constructor Create;
    destructor Destroy; override;

    procedure clear;

    property PostWSAAcceptExCounter: Integer read FPostWSAAcceptExCounter;
    property PostWSARecvCounter: Integer read FPostWSARecvCounter;
    property PostWSASendCounter: Integer read FPostWSASendCounter;
    property PostWSASendSize: Int64 read FPostWSASendSize;
    property RecvSize: Int64 read FRecvSize;
    property ResponseWSAAcceptExCounter: Integer read FResponseWSAAcceptExCounter;
    property ResponseWSARecvCounter: Integer read FResponseWSARecvCounter;
    property ResponseWSASendCounter: Integer read FResponseWSASendCounter;
    property SentSize: Int64 read FSentSize;
  end;

  TContextDoublyLinked = class(TObject)
  private
    FLocker: TIocpLocker;
    FHead:TIocpClientContext;
    FTail:TIocpClientContext;
    FCount:Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure add(pvContext:TIocpClientContext);
    procedure remove(pvContext:TIocpClientContext);

    property Count: Integer read FCount;

  end;


  TIocpTcpServer = class(TComponent)
  public
    // clientContext pool
    FContextPool: TBaseQueue;


    // sendRequest pool
    FSendRequestPool: TBaseQueue;

    FClientContextClass:TIocpClientContextClass;

    /// data record
    FDataMoniter: TIocpDataMonitor;

    FActive: Boolean;


    // online clientcontext list
    FOnlineContextList: TContextDoublyLinked;

    // acceptEx request mananger
    FIocpAcceptorMgr:TIocpAcceptorMgr;

    FIocpEngine: TIocpEngine;

    FKeepAlive: Boolean;

    // server listen socket, accept client connection
    FListenSocket: TRawSocket;


    FOnDataReceived: TOnDataReceived;


    FOnClientContextError: TOnClientContextError;

    FPort: Integer;

    procedure DoClientContextError(pvClientContext: TIocpClientContext;
        pvErrorCode: Integer);
    function GetWorkerCount: Integer;

    procedure SetWorkerCount(const Value: Integer);

    procedure SetActive(pvActive:Boolean);





    /// <summary>
    ///   occur on create instance
    /// </summary>
    procedure onCreateClientContext(const context: TIocpClientContext);virtual;

    /// <summary>
    ///   pop ClientContext object
    /// </summary>
    function getClientContext():TIocpClientContext;

    /// <summary>
    ///   giveback to pool
    /// </summary>
    function releaseClientContext(pvObject:TIocpClientContext): Boolean;


    /// <summary>
    ///   pop sendRequest object
    /// </summary>
    function getSendRequest():TIocpSendRequest;

    /// <summary>
    ///   push back to pool
    /// </summary>
    function releaseSendRequest(pvObject:TIocpSendRequest): Boolean;


    procedure doReceiveData(pvIocpClientContext:TIocpClientContext; pvRequest:TIocpRecvRequest);
  public
    procedure DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);

    function GetClientCount: Integer;

  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    /// <summary>
    ///   create DataMonitor object
    /// </summary>
    procedure createDataMonitor;




    /// <summary>
    ///   check clientContext object is valid.
    /// </summary>
    function checkClientContextValid(const pvClientContext: TIocpClientContext):  Boolean;

    /// <summary>
    ///
    /// </summary>
    procedure DisconnectAll;

    /// <summary>
    ///   stop and wait all workers down
    /// </summary>
    procedure safeStop();

    property Active: Boolean read FActive write SetActive;

    /// <summary>
    ///   client connections counter
    /// </summary>
    property ClientCount: Integer read GetClientCount;
    property DataMoniter: TIocpDataMonitor read FDataMoniter;

    /// <summary>
    ///   set socket Keep alive option when acceptex
    /// </summary>
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
  published
    /// <summary>
    ///   listen port
    /// </summary>
    property Port: Integer read FPort write FPort;

    /// <summary>
    ///   default cpu count * 2 -1
    /// </summary>
    property WorkerCount: Integer read GetWorkerCount write SetWorkerCount;


    /// <summary>
    ///  on work error
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property OnClientContextError: TOnClientContextError read FOnClientContextError
        write FOnClientContextError;

    /// <summary>
    ///  on clientcontext received data
    ///    called by iocp worker thread
    /// </summary>
    property OnDataReceived: TOnDataReceived read FOnDataReceived write
        FOnDataReceived;
  end;

implementation


/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean; overload;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;


procedure TIocpClientContext.DoDisconnect;
begin
  if lock_cmp_exchange(True, False, FActive) then
  begin
    FRawSocket.close;
    checkReleaseRes;
    if FOwner <> nil then
    begin
      FOwner.FOnlineContextList.remove(self);
      FOwner.releaseClientContext(Self);
    end else
    begin
      self.Free;
    end;
  end else
  begin
    FActive := FActive;
  end;
end;

procedure TIocpClientContext.checkNextSendRequest;
var
  lvRequest:TIocpSendRequest;
begin
  lvRequest := TIocpSendRequest(FSendRequestLink.Pop);
  if lvRequest <> nil then
  begin
    uiLogger.logMessage('Popup counter:%d', [FSendRequestLink.Count]);
    lvRequest.start;

  end else
  begin  // no request to send
    uiLogger.logMessage('Popup empty:%d', [FSendRequestLink.Count]);
    FSending := false;
  end;
end;

procedure TIocpClientContext.checkReleaseRes;
var
  lvRequest:TIocpSendRequest;
begin
  if FSendRequestLink.Count > 9 then
  begin
    DebugINfo := Format('empty link: %d', [FSendRequestLink.Count]);
  end;
  //DebugINfo := Format('empty link: %d', [FSendRequestLink.Count]);
  while true do
  begin
    lvRequest :=TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest <> nil then
    begin
      FOwner.releaseSendRequest(lvRequest);
    end else
    begin
      Break;
    end;
  end;
end;

procedure TIocpClientContext.checkRequestObject;
begin
  if FRecvRequest = nil then
  begin
    FRecvRequest := TIocpRecvRequest.Create;
    FRecvRequest.FClientContext := self;
    FRecvRequest.FOwner := FOwner;
  end;
end;

constructor TIocpClientContext.Create;
begin
  inherited Create;
  FAlive := False;
  FRawSocket := TRawSocket.Create();
  FActive := false;
  FSendRequestLink := TIocpRequestSingleLink.Create(10);
end;

destructor TIocpClientContext.Destroy;
begin

  FRawSocket.close;
  FRawSocket.Free;

  FRecvRequest.Free;

  Assert(FSendRequestLink.Count = 0);

  FSendRequestLink.Free;
  
  inherited Destroy;
end;

procedure TIocpClientContext.DoCleanUp;
begin
  FOwner := nil;
  if FActive then
  begin
    FRawSocket.close;
    FActive := false;
    checkReleaseRes;
  end;
end;

procedure TIocpClientContext.DoConnected;
begin
  if lock_cmp_exchange(False, True, FActive) = False then
  begin
    checkRequestObject;
    PostWSARecvRequest;
    if FOwner <> nil then
    begin
      FOwner.FOnlineContextList.add(Self);
    end;
  end else
  begin
    FActive := FActive;
  end;
end;

procedure TIocpClientContext.DoReceiveData;
begin
  OnRecvBuffer(FRecvRequest.FPostBuffer.buf,
    FRecvRequest.FBytesTransferred,
    FRecvRequest.FErrorCode);
  if FOwner <> nil then
    FOwner.doReceiveData(Self, FRecvRequest);
end;

procedure TIocpClientContext.DoSendRequestCompleted(pvRequest:
    TIocpSendRequest);
begin
  ;
end;

procedure TIocpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode:
    WORD);
begin
  
end;

procedure TIocpClientContext.PostWSARecvRequest;
begin
  FRecvRequest.PostRequest;
end;



function TIocpClientContext.PostWSASendRequest(buf: Pointer; len: Cardinal): Boolean;
var
  lvRequest:TIocpSendRequest;
begin

  lvRequest := FOwner.getSendRequest;
  Assert(lvRequest <> nil);

  lvRequest.FClientContext := self;
  lvRequest.setBuffer(buf, len);

  uiLogger.logMessage('will push :%d', [FSendRequestLink.Count]);
  if FSendRequestLink.Push(lvRequest) then
  begin
    Result := true;

    if lock_cmp_exchange(False, True, FSending) = False then
    begin
      // start sending
      checkNextSendRequest;
    end;
  end else
  begin
    self.DebugINfo := 'PostWSASendRequest push fail';
    FOwner.releaseSendRequest(lvRequest);
    DoDisconnect;
    Result := false;
  end;
end;

procedure TIocpClientContext.SetDebugINfo(const Value: string);
begin
  FDebugINfo := Value;
  uiLogger.logMessage(FDebugINfo);
end;

function TIocpTcpServer.checkClientContextValid(const pvClientContext: TIocpClientContext): Boolean;
begin
  Result := (pvClientContext.FOwner = Self);
  //Result := FOnlineContextList.IndexOf(pvClientContext) <> -1;
end;

constructor TIocpTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeepAlive := true;
  FContextPool := TBaseQueue.Create;
  FSendRequestPool := TBaseQueue.Create;
    
  FIocpEngine := TIocpEngine.Create();
  FOnlineContextList := TContextDoublyLinked.Create();
  FListenSocket := TRawSocket.Create;
  FIocpAcceptorMgr := TIocpAcceptorMgr.Create(Self);
end;

destructor TIocpTcpServer.Destroy;
begin
  safeStop;

  if FDataMoniter <> nil then FDataMoniter.Free;

  FContextPool.FreeDataObject;
  FSendRequestPool.FreeDataObject;

  FListenSocket.Free;
  FIocpAcceptorMgr.Free;
  FIocpEngine.Free;

  FOnlineContextList.Free;

  FContextPool.Free;
  FSendRequestPool.Free;
  inherited Destroy;
end;

procedure TIocpTcpServer.DisconnectAll;
var
  i:Integer;
begin
//  FClientContextLocker.lock;
//  try
//    for I := FOnlineContextList.Count -1 downto 0  do
//    begin
//      TIocpClientContext(FOnlineContextList[i]).DoDisconnect;
//    end;
//  finally
//    FClientContextLocker.unLock;
//  end;
end;

procedure TIocpTcpServer.DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);
var
  lvRet, lvErrCode:Integer;
  lvContinue:Boolean;
begin
  if pvRequest.FErrorCode = 0 then
  begin
    lvContinue := true;
    if FKeepAlive then
    begin
      if not pvRequest.FClientContext.FRawSocket.setKeepAliveOption() then
      begin       // set option fail
        lvErrCode := GetLastError;
        DoClientContextError(nil, lvErrCode);

        pvRequest.FClientContext.FRawSocket.close;

        // relase client context object
        releaseClientContext(pvRequest.FClientContext);
        pvRequest.FClientContext := nil;
        
        lvContinue := false;
      end;
    end;

    if lvContinue then
    begin
      lvRet := FIocpEngine.IocpCore.bind2IOCPHandle(pvRequest.FClientContext.FRawSocket.SocketHandle, 0);
      if lvRet = 0 then
      begin     // binding error
        lvErrCode := GetLastError;
        DoClientContextError(nil, lvErrCode);

        pvRequest.FClientContext.FRawSocket.close;

        // relase client context object
        releaseClientContext(pvRequest.FClientContext);
        pvRequest.FClientContext := nil;
      end else
      begin
        pvRequest.FClientContext.DoConnected;
      end;
    end;
  end else
  begin
    // relase client context object
    releaseClientContext(pvRequest.FClientContext);
    pvRequest.FClientContext := nil;
  end;

  // request will free or return to pool
  FIocpAcceptorMgr.removeRequestObject(pvRequest);

  if FActive then FIocpAcceptorMgr.checkPostRequest;
end;

procedure TIocpTcpServer.DoClientContextError(pvClientContext:
    TIocpClientContext; pvErrorCode: Integer);
begin
  if Assigned(FOnClientContextError) then
    FOnClientContextError(pvClientContext, pvErrorCode);
end;

procedure TIocpTcpServer.doReceiveData(pvIocpClientContext: TIocpClientContext;
  pvRequest: TIocpRecvRequest);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(pvIocpClientContext,
      pvRequest.FPostBuffer.buf, pvRequest.FPostBuffer.len,
      pvRequest.FErrorCode);
end;

function TIocpTcpServer.getClientContext: TIocpClientContext;
begin
  Result := TIocpClientContext(FContextPool.Pop);
  if Result = nil then
  begin
    if FClientContextClass <> nil then
    begin
      Result := FClientContextClass.Create;
      onCreateClientContext(Result);
    end else
    begin
      Result := TIocpClientContext.Create;
      onCreateClientContext(Result);
    end;
  end;
  Result.FAlive := True;
  Result.DoCleanUp;
  Result.FOwner := Self;
end;

function TIocpTcpServer.GetWorkerCount: Integer;
begin
  Result := FIocpEngine.WorkerCount;
end;

procedure TIocpTcpServer.onCreateClientContext(
  const context: TIocpClientContext);
begin

end;

function TIocpTcpServer.releaseClientContext(pvObject:TIocpClientContext):
    Boolean;
begin
  if lock_cmp_exchange(True, False, pvObject.FAlive) = true then
  begin
    pvObject.DoCleanUp;
    FContextPool.Push(pvObject);
    Result := true;
  end else
  begin
    Result := false;
  end;
end;

function TIocpTcpServer.releaseSendRequest(pvObject:TIocpSendRequest): Boolean;
begin
  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then
  begin
    FSendRequestPool.Push(pvObject);
    Result := true;
  end else
  begin
    Result := false;
  end;
end;

procedure TIocpTcpServer.safeStop;
begin
  if FActive then
  begin
    FActive := false;

    // close listen socket
    FListenSocket.close;

    DisconnectAll;

    // engine stop
    FIocpEngine.safeStop;    
  end; 
end;

procedure TIocpTcpServer.SetActive(pvActive:Boolean);
begin
  if pvActive <> FActive then
  begin
    if pvActive then
    begin
      if FDataMoniter <> nil then FDataMoniter.clear;
      
      // engine start
      FIocpEngine.checkStart;
      
      // create listen socket
      FListenSocket.createTcpOverlappedSocket;

      // bind to listen port
      if not FListenSocket.bind('', FPort) then
      begin
        RaiseLastOSError;
      end;

      // listen
      if not FListenSocket.listen() then
      begin
        RaiseLastOSError;
      end;

      FIocpEngine.IocpCore.bind2IOCPHandle(FListenSocket.SocketHandle, 0);


      FIocpAcceptorMgr.FMinRequest := 10;
      FIocpAcceptorMgr.FMaxRequest := 100;

      // post AcceptEx request
      FIocpAcceptorMgr.checkPostRequest;

      FActive := True;
    end else
    begin
      safeStop;
    end; 
  end;
end;

procedure TIocpTcpServer.SetWorkerCount(const Value: Integer);
begin
  FIocpEngine.setWorkerCount(Value);
end;

procedure TIocpTcpServer.createDataMonitor;
begin
  if FDataMoniter = nil then
  begin
    FDataMoniter := TIocpDataMonitor.Create;
  end;
end;

function TIocpTcpServer.GetClientCount: Integer;
begin
  Result := FOnlineContextList.FCount;
end;

function TIocpTcpServer.getSendRequest: TIocpSendRequest;
begin
  Result := TIocpSendRequest(FSendRequestPool.Pop);
  if Result = nil then
  begin
    Result := TIocpSendRequest.Create;
  end;
//  if Result.FAlive then
//  begin
//    Result.FAlive := true;
//  end;
  Result.FAlive := true;
  Result.DoCleanup;
  Result.FOwner := Self;
end;

procedure TIocpAcceptorMgr.checkPostRequest;
var
  lvRequest:TIocpAcceptExRequest;
begin
  FLocker.lock;
  try
    if FList.Count > FMinRequest then Exit;

    // post request
    while FList.Count < FMaxRequest do
    begin
      lvRequest := TIocpAcceptExRequest.Create(FOwner);
      lvRequest.FClientContext := FOwner.getClientContext;
      lvRequest.FAcceptorMgr := self;
      FList.Add(lvRequest);
      lvRequest.PostRequest;

      if FOwner.FDataMoniter <> nil then
      begin
        InterlockedIncrement(FOwner.FDataMoniter.FPostWSAAcceptExCounter);
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpAcceptorMgr.Create(AOwner: TIocpTcpServer);
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'acceptorLocker';
  FMaxRequest := 200;
  FMinRequest := 10;  
  FList := TList.Create;
  FOwner := AOwner;
end;

destructor TIocpAcceptorMgr.Destroy;
begin
  FList.Free;
  FLocker.Free;
  inherited Destroy;
end;

procedure TIocpAcceptorMgr.notifyForDestory;
var
  i:Integer;
  lvRequest:TIocpAcceptExRequest;
begin
  FLocker.lock;
  try
    for i := 0 to FList.Count - 1 do
    begin
      lvRequest := TIocpAcceptExRequest(FList[i]);
      lvRequest.FOwner := nil;
      lvRequest.FAcceptorMgr := nil;
    end;
    FList.Clear;
  finally
    FLocker.unLock;
  end;
end;

procedure TIocpAcceptorMgr.releaseRequestObject(
  pvRequest: TIocpAcceptExRequest);
begin
  
  pvRequest.Free;

end;

procedure TIocpAcceptorMgr.removeRequestObject(pvRequest: TIocpAcceptExRequest);
begin
  FLocker.lock;
  try
    FList.Remove(pvRequest);
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpAcceptExRequest.Create(AOwner: TIocpTcpServer);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TIocpAcceptExRequest.getPeerINfo;
var
  localAddr: PSockAddr;
  remoteAddr: PSockAddr;
  localAddrSize : Integer;
  remoteAddrSize : Integer;
begin
  localAddrSize := SizeOf(TSockAddr) + 16;
  remoteAddrSize := SizeOf(TSockAddr) + 16;
  IocpGetAcceptExSockaddrs(@FAcceptBuffer[0],
                        0,
                        SizeOf(localAddr^) + 16,
                        SizeOf(remoteAddr^) + 16,
                        localAddr,
                        localAddrSize,
                        remoteAddr,
                        remoteAddrSize);

  FClientContext.FRemoteAddr := string(inet_ntoa(TSockAddrIn(remoteAddr^).sin_addr));
  FClientContext.FRemotePort := ntohs(TSockAddrIn(remoteAddr^).sin_port);


//  getpeername(FRawSocket.SocketHandle, TSockAddr(SockAddrIn), Size);
//
//  FRemoteAddr := inet_ntoa(SockAddrIn.sin_addr);
//  FRemotePort := ntohs(SockAddrIn.sin_port);
end;

procedure TIocpAcceptExRequest.HandleResponse;
begin
  if FOwner = nil then
  begin        // owner is shutdown
    if FClientContext <> nil then FClientContext.Free;
    Self.Free;
  end else
  begin
    ///
    if FOwner.FDataMoniter <> nil then
    begin
      InterlockedIncrement(FOwner.FDataMoniter.FResponseWSAAcceptExCounter);
    end;

    try
      getPeerINfo();
      FOwner.DoAcceptExResponse(Self);
    finally
      FAcceptorMgr.releaseRequestObject(Self);
    end;
  end;
end;

function TIocpAcceptExRequest.PostRequest: Boolean;
var
  dwBytes: Cardinal;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:POverlapped;
begin
  FClientContext.FRawSocket.createTcpOverlappedSocket;
  dwBytes := 0;
  lp := @FOverlapped;
  lvRet := IocpAcceptEx(FOwner.FListenSocket.SocketHandle
                , FClientContext.FRawSocket.SocketHandle
                , @FAcceptBuffer[0]
                , 0
                , SizeOf(TSockAddrIn) + 16
                , SizeOf(TSockAddrIn) + 16
                , dwBytes
                , lp);
  if not lvRet then
  begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then
    begin
      FOwner.DoClientContextError(FClientContext, lvErrCode);
    end;
  end else
  begin
    Result := True;
  end;
end;

constructor TIocpRecvRequest.Create;
begin
  inherited Create;
  GetMem(FInnerBuffer.buf, buf_size);
  FInnerBuffer.len := buf_size;
end;

destructor TIocpRecvRequest.Destroy;
begin
  FreeMem(FInnerBuffer.buf, buf_size);
  inherited Destroy;
end;

procedure TIocpRecvRequest.HandleResponse;
begin
  FClientContext.DebugINfo := 'TIocpRecvRequest.HandleResponse starting';

  if FOwner = nil then
  begin
    FClientContext.Free;
    exit;
  end else if (FOwner.FDataMoniter <> nil) then
  begin
    FOwner.FDataMoniter.incResponseWSARecvCounter;
    FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
  end;

  if FErrorCode <> 0 then
  begin
    FClientContext.DebugINfo := 'TIocpRecvRequest.HandleResponse FErrorCode <> 0';
    if FOwner <> nil then
    begin
      FOwner.DoClientContextError(FClientContext, FErrorCode);
      FClientContext.DoDisconnect;
    end;
  end else if (FBytesTransferred = 0) then
  begin      // no data recvd, socket is break
    FClientContext.DebugINfo := 'TIocpRecvRequest.HandleResponse FBytesTransferred = 0';
    FClientContext.DoDisconnect;
  end else
  begin
    try
      FClientContext.DebugINfo := 'TIocpRecvRequest.HandleResponse DoReceiveData';
      FClientContext.DoReceiveData;
    finally
      // post recv request
      FClientContext.DebugINfo := 'TIocpRecvRequest.HandleResponse DoReceiveData ending';
      if FOwner = nil then
      begin
        FClientContext.Free;
      end else if FOwner.Active then
      begin
        if FClientContext.FActive then
        begin
          // post recv request again
          FClientContext.PostWSARecvRequest;
        end;
      end else
      begin
        FClientContext.DoDisconnect;
      end;
    end;
  end;
  FClientContext.DebugINfo := 'TIocpRecvRequest.HandleResponse ending';
end;

function TIocpRecvRequest.PostRequest(pvBuffer: PAnsiChar;
  len: Cardinal): Boolean;
var
  lvRet:Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;

  FPostBuffer.buf := pvBuffer;
  FPostBuffer.len := len;

  lvRet := iocpWinsock2.WSARecv(FClientContext.FRawSocket.SocketHandle,
     @FPostBuffer,
     1,
     lpNumberOfBytesRecvd,
     FRecvdFlag,
     @FOverlapped,
     nil
     );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
      // trigger error event
      FOwner.DoClientContextError(FClientContext, lvRet);

      // kick out clientContext
      FClientContext.DoDisconnect;
    end else
    begin
      if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostWSARecvCounter;
      end;
    end;
  end else
  begin
    Result := True;
    
    if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPostWSARecvCounter;
    end;
  end;
  
end;

function TIocpRecvRequest.PostRequest: Boolean;
begin
  FInnerBuffer.len := buf_size;
  Result := PostRequest(FInnerBuffer.buf, FInnerBuffer.len);
end;

procedure TIocpSendRequest.checkSendNextBlock;
var
  l:Cardinal;
begin
  if FPosition < FLen then
  begin
    l := FLen - FPosition;
    if l > MAX_SEND_BLOCK_SIZE then l := MAX_SEND_BLOCK_SIZE;
    InnerPostRequest(Pointer(IntPtr(FBuf) + IntPtr(FPosition)), l);
  end;
end;

constructor TIocpSendRequest.Create;
begin
  inherited Create;
  FReponseCounter := 0;
  FSendCounter := 0;
end;

destructor TIocpSendRequest.Destroy;
begin
  inherited Destroy;
end;

procedure TIocpSendRequest.DoCleanUp;
begin
  FBytesSize := 0;
  FSendCounter := 0;
  FReponseCounter := 0;
  FNext := nil;
  FOwner := nil;
  FClientContext := nil;
  FBuf := nil;
  FLen := 0;
  FPosition := 0;
end;

procedure TIocpSendRequest.HandleResponse;
begin
  if FOwner = nil then
  begin
    FClientContext.Free;
    Self.Free;
    exit;
  end else if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
  begin

    ///    if FBytesTransferred <> FWSABuf.len then  /// error
    FOwner.FDataMoniter.incSentSize(FWSABuf.len);
    FOwner.FDataMoniter.incResponseWSASendCounter;
  end;

  if FErrorCode <> 0 then
  begin
    FOwner.DoClientContextError(FClientContext, FErrorCode);
    FClientContext.DoDisconnect;
  end else
  begin
    Inc(FPosition, FBytesTransferred);
    if FPosition = FLen then
    begin        // all sent ?
      if Assigned(FOnDataRequestCompleted) then
      begin
        FOnDataRequestCompleted(FClientContext, Self);
      end;

      FClientContext.DoSendRequestCompleted(Self);

      FClientContext.checkNextSendRequest;

      // release request
      FOwner.releaseSendRequest(Self);
    end else
    begin
      checkSendNextBlock;
    end;
  end;
  FClientContext.DebugINfo := 'TIocpSendRequest.HandleResponse end';
end;

function TIocpSendRequest.InnerPostRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvRet: Integer;
  dwFlag: Cardinal;
  lpNumberOfBytesSent:Cardinal;
begin
  FBytesSize := len;
  FWSABuf.buf := buf;
  FWSABuf.len := len;
  dwFlag := 0;
  lpNumberOfBytesSent := 0;
  lvRet := WSASend(FClientContext.FRawSocket.SocketHandle,
                    @FWSABuf,
                    1,
                    lpNumberOfBytesSent,
                    dwFlag,
                    @FOverlapped,
                    nil
  );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
       FOwner.DoClientContextError(FClientContext, lvRet);

       /// kick out the clientContext
       FClientContext.DoDisconnect;
    end else
    begin
      if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostWSASendSize(len);
        FOwner.FDataMoniter.incPostWSASendCounter;
      end;
    end;
  end else
  begin
    Result := True;
    if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPostWSASendSize(len);
      FOwner.FDataMoniter.incPostWSASendCounter;
    end;
  end;
end;

procedure TIocpSendRequest.setBuffer(buf: Pointer; len: Cardinal);
begin
  FBuf := buf;
  FLen := len;
  FPosition := 0;
end;

procedure TIocpSendRequest.start;
begin
  checkSendNextBlock;
end;

procedure TIocpDataMonitor.clear;
begin
  FLocker.Enter;
  try
    FSentSize:=0;
    FRecvSize:=0;
    FPostWSASendSize:=0;

    FPostWSASendCounter:=0;
    FResponseWSASendCounter:=0;

    FPostWSARecvCounter:=0;
    FResponseWSARecvCounter:=0;

    FPostWSAAcceptExCounter:=0;
    FResponseWSAAcceptExCounter:=0;
  finally
    FLocker.Leave;
  end;
end;

constructor TIocpDataMonitor.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
end;

destructor TIocpDataMonitor.Destroy;
begin
  FLocker.Free;
  inherited Destroy;
end;

procedure TIocpDataMonitor.incPostWSARecvCounter;
begin
  InterlockedIncrement(FPostWSARecvCounter);
end;

procedure TIocpDataMonitor.incPostWSASendCounter;
begin
  InterlockedIncrement(FPostWSASendCounter);
end;

procedure TIocpDataMonitor.incPostWSASendSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  try
    FPostWSASendSize := FPostWSASendSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpDataMonitor.incRecvdSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  try
    FRecvSize := FRecvSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpDataMonitor.incResponseWSARecvCounter;
begin
  InterlockedIncrement(FResponseWSARecvCounter);
end;

procedure TIocpDataMonitor.incResponseWSASendCounter;
begin
  InterlockedIncrement(FResponseWSASendCounter);
end;

procedure TIocpDataMonitor.incSentSize(pvSize:Cardinal);
begin
  FLocker.Enter;
  try
    FSentSize := FSentSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TContextDoublyLinked.add(pvContext: TIocpClientContext);
begin
  FLocker.lock;
  try
    if FHead = nil then
    begin
      FHead := pvContext;
    end else
    begin
      FTail.FNext := pvContext;
      pvContext.FPre := FTail;
    end;

    FTail := pvContext;
    FTail.FNext := nil;

    inc(FCount);
  finally
    FLocker.unLock;
  end;
end;

constructor TContextDoublyLinked.Create;
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'onlineContext';
  FHead := nil;
  FTail := nil;
end;

destructor TContextDoublyLinked.Destroy;
begin
  FreeAndNil(FLocker);
  inherited Destroy;
end;

procedure TContextDoublyLinked.remove(pvContext: TIocpClientContext);
begin
  FLocker.lock;
  try
    if pvContext.FPre <> nil then
    begin
      pvContext.FPre.FNext := pvContext.FNext;
      if pvContext.FNext <> nil then
        pvContext.FNext.FPre := pvContext.FPre;
    end else
    begin    // pre is nil, pvContext is FHead
      if pvContext.FNext <> nil then
        pvContext.FNext.FPre := nil;
      FHead := pvContext.FNext;
    end;
    Dec(FCount);

    //  set pvConext.FPre is FTail
    if FTail = pvContext then FTail := pvContext.FPre;


    pvContext.FPre := nil;
    pvContext.FNext := nil;
  finally
    FLocker.unLock;
  end;
end;

end.
