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
  BaseQueue;

const
  buf_size = 1024 * 50;

type
  TIocpTcpServer = class;
  TIocpAcceptorMgr = class;
  TIocpClientContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;

  TIocpClientContextClass = class of TIocpClientContext;

  TOnWorkError = procedure(Sender:TObject; errCode:Integer) of object;

  TOnDataReceived = procedure(pvClientContext:TIocpClientContext;
      buf:Pointer; len:cardinal; errCode:Integer) of object;

  TOnClientContextConnect = procedure(pvClientContext:TIocpClientContext; errCode:Integer) of object;

  /// <summary>
  ///   client object
  /// </summary>
  TIocpClientContext = class(TObject)
  private
    FActive: Boolean;
    FOwner: TIocpTcpServer;
    FRecvRequest:TIocpRecvRequest;
    FSendRequest:TIocpSendRequest;
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
    procedure DoSendRequestCompleted;

    /// <summary>
    ///   request recv data
    /// </summary>
    procedure PostWSARecvRequest();


  protected
    procedure DoConnected;virtual;

    procedure DoCleanUp;virtual;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Disconnect;
    
    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf:Pointer; len:Cardinal):Boolean;

    property Active: Boolean read FActive;

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
    /// sending buffer
    FSending:Boolean;

    FWokeBufferSize: Cardinal;
    
    FOwner: TIocpTcpServer;

    FClientContext:TIocpClientContext;
    
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;


    /// <summary>
    ///   post send buffer to iocp queue
    /// </summary>
    function postSendBufferRequest(buf: Pointer; len: Cardinal): Boolean;
  public
    constructor Create;

    destructor Destroy; override;
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
    FLocker: TCriticalSection;
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
    FSentSize:Cardinal;
    FRecvSize:Cardinal;
    FPostWSASendSize: Cardinal;

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
    property PostWSASendSize: Cardinal read FPostWSASendSize;
    property RecvSize: Cardinal read FRecvSize;
    property ResponseWSAAcceptExCounter: Integer read FResponseWSAAcceptExCounter;
    property ResponseWSARecvCounter: Integer read FResponseWSARecvCounter;
    property ResponseWSASendCounter: Integer read FResponseWSASendCounter;
    property SentSize: Cardinal read FSentSize;
  end;


  TIocpTcpServer = class(TComponent)
  private
    // clientContext pool
    FContextPool: TBaseQueue;

    FClientContextClass:TIocpClientContextClass;

    /// data record
    FDataMoniter: TIocpDataMonitor;

    FActive: Boolean;

    FClientContextLocker:TCriticalSection;

    // online clientcontext list
    FOnlineClientContextList: TList;

    // acceptEx request mananger
    FIocpAcceptorMgr:TIocpAcceptorMgr;

    FIocpEngine: TIocpEngine;

    // server listen socket, accept client connection
    FListenSocket: TRawSocket;


    FOnDataReceived: TOnDataReceived;


    FOnError: TOnWorkError;

    FPort: Integer;

    procedure doError(pvErrorCode:Integer);
    function GetWorkerCount: Integer;

    procedure SetWorkerCount(const Value: Integer);

    procedure SetActive(pvActive:Boolean);



    /// <summary>
    ///   remove clientContext from online list
    /// </summary>
    function removeClientContext(pvObject: TIocpClientContext): Boolean;

    /// <summary>
    ///   add clientContext to onlineList
    /// </summary>
    procedure addClientContext(pvObject:TIocpClientContext);


    /// <summary>
    ///   occur on create instance
    /// </summary>
    procedure onCreateClientContext(const context: TIocpClientContext);virtual;

    /// <summary>
    ///   get a ClientContext object
    /// </summary>
    function getClientContext():TIocpClientContext;

    /// <summary>
    ///   clientContext object will be free, or giveback to pool
    /// </summary>
    procedure releaseClientContext(pvObject:TIocpClientContext);

    procedure notifyForDestory;

    procedure doReceiveData(pvIocpClientContext:TIocpClientContext; pvRequest:TIocpRecvRequest);
  private
    procedure DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);
    function GetClientCount: Integer;

    /// <summary>
    ///
    /// </summary>
    procedure DisconnectAll;
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
    function checkClientContextValid(const pvClientContext: TIocpClientContext):
        Boolean;

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
    property OnError: TOnWorkError read FOnError write FOnError;

    /// <summary>
    ///  on clientcontext received data
    ///    called by iocp worker thread
    /// </summary>
    property OnDataReceived: TOnDataReceived read FOnDataReceived write
        FOnDataReceived;
  end;

implementation


procedure TIocpClientContext.Disconnect;
begin
  FRawSocket.close;
  FActive := false;
end;

procedure TIocpClientContext.checkRequestObject;
begin
  if FRecvRequest = nil then
  begin
    FRecvRequest := TIocpRecvRequest.Create;
    FRecvRequest.FClientContext := self;
    FRecvRequest.FOwner := FOwner;
  end;

  if FSendRequest = nil then
  begin
    FSendRequest := TIocpSendRequest.Create;
    FSendRequest.FClientContext := self;
    FSendRequest.FOwner := FOwner;
  end;
end;

constructor TIocpClientContext.Create;
begin
  inherited Create;
  FRawSocket := TRawSocket.Create();
  FActive := false;
end;

destructor TIocpClientContext.Destroy;
begin
  FRawSocket.close;
  FRawSocket.Free;

  FSendRequest.Free;
  FRecvRequest.Free;
  inherited Destroy;
end;

procedure TIocpClientContext.DoCleanUp;
begin
  FOwner := nil;
  FRawSocket.close;
  FActive := false;
end;

procedure TIocpClientContext.DoConnected;
begin
  checkRequestObject;
  PostWSARecvRequest;
end;

procedure TIocpClientContext.DoReceiveData;
begin
  OnRecvBuffer(FRecvRequest.FPostBuffer.buf,
    FRecvRequest.FBytesTransferred,
    FRecvRequest.FErrorCode);

  FOwner.doReceiveData(Self, FRecvRequest);
end;

procedure TIocpClientContext.DoSendRequestCompleted;
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



function TIocpClientContext.PostWSASendRequest(buf: Pointer;
  len: Cardinal): Boolean;
begin
  if not FSendRequest.FSending then
  begin
    Result := FSendRequest.postSendBufferRequest(buf, len);
  end else
  begin
    Result := False;
  end;
end;

procedure TIocpTcpServer.addClientContext(pvObject: TIocpClientContext);
begin
  FClientContextLocker.Enter;
  try
    FOnlineClientContextList.Add(pvObject);
  finally
    FClientContextLocker.Leave;
  end;  
end;

function TIocpTcpServer.checkClientContextValid(const pvClientContext: TIocpClientContext): Boolean;
begin
  Result := (pvClientContext.FOwner = Self);
  //Result := FOnlineClientContextList.IndexOf(pvClientContext) <> -1;
end;

constructor TIocpTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContextPool := TBaseQueue.Create;
  
  FClientContextLocker := TCriticalSection.Create;
  FIocpEngine := TIocpEngine.Create();
  FOnlineClientContextList := TList.Create();
  FListenSocket := TRawSocket.Create;  
  FIocpAcceptorMgr := TIocpAcceptorMgr.Create(Self);
end;

destructor TIocpTcpServer.Destroy;
begin
  safeStop;

  if FDataMoniter <> nil then FDataMoniter.Free;

  FContextPool.FreeDataObject;

  FListenSocket.Free;
  FIocpAcceptorMgr.Free;
  FIocpEngine.Free;
  FOnlineClientContextList.Free;
  FClientContextLocker.Free;


  FContextPool.Free;
  inherited Destroy;
end;

procedure TIocpTcpServer.DisconnectAll;
var
  i:Integer;
begin
  FClientContextLocker.Enter;
  try
    for I := 0 to FOnlineClientContextList.Count -1 do
    begin
      TIocpClientContext(FOnlineClientContextList[i]).Disconnect;
    end;
  finally
    FClientContextLocker.Leave;
  end;
end;

procedure TIocpTcpServer.DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);
var
  lvRet, lvErrCode:Integer;
begin
  if pvRequest.FErrorCode = 0 then
  begin
    lvRet := FIocpEngine.IocpCore.bind2IOCPHandle(pvRequest.FClientContext.FRawSocket.SocketHandle, 0);
    if lvRet = 0 then
    begin     // binding error
      lvErrCode := GetLastError;
      doError(lvErrCode);

      pvRequest.FClientContext.FRawSocket.close;

      // relase client context object
      releaseClientContext(pvRequest.FClientContext);
      pvRequest.FClientContext := nil;
    end else
    begin
      // add to online list
      addClientContext(pvRequest.FClientContext);

      pvRequest.FClientContext.FActive := true;
      pvRequest.FClientContext.DoConnected;
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

procedure TIocpTcpServer.doError(pvErrorCode: Integer);
begin
  if Assigned(FOnError) then
    FOnError(self, pvErrorCode);
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
  end else
  begin
    Result.DoCleanUp;
  end;
  Result.FOwner := Self;
end;

function TIocpTcpServer.GetWorkerCount: Integer;
begin
  Result := FIocpEngine.WorkerCount;
end;

procedure TIocpTcpServer.notifyForDestory;
var
  i:Integer;
begin
  FClientContextLocker.Enter;
  try
    for I := 0 to FOnlineClientContextList.Count -1 do
    begin
      // unbinding relations
      TIocpClientContext(FOnlineClientContextList[i]).FOwner := nil;
    end;

    FOnlineClientContextList.Clear;
  finally
    FClientContextLocker.Leave;
  end;

end;

procedure TIocpTcpServer.onCreateClientContext(
  const context: TIocpClientContext);
begin

end;

procedure TIocpTcpServer.releaseClientContext(pvObject: TIocpClientContext);
begin
  pvObject.DoCleanUp;
  FContextPool.Push(pvObject);
end;

function TIocpTcpServer.removeClientContext(pvObject: TIocpClientContext):
    Boolean;
begin
  FClientContextLocker.Enter;
  try
    Result := FOnlineClientContextList.Remove(pvObject) > 0;
  finally
    FClientContextLocker.Leave;
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
  Result := FOnlineClientContextList.Count;
end;

procedure TIocpAcceptorMgr.checkPostRequest;
var
  lvRequest:TIocpAcceptExRequest;
begin
  FLocker.Enter;
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
    FLocker.Leave;
  end;
end;

constructor TIocpAcceptorMgr.Create(AOwner: TIocpTcpServer);
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
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
  FLocker.Enter;
  try
    for i := 0 to FList.Count - 1 do
    begin
      lvRequest := TIocpAcceptExRequest(FList[i]);
      lvRequest.FOwner := nil;
      lvRequest.FAcceptorMgr := nil;
    end;
    FList.Clear;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpAcceptorMgr.releaseRequestObject(
  pvRequest: TIocpAcceptExRequest);
begin
  
  pvRequest.Free;

end;

procedure TIocpAcceptorMgr.removeRequestObject(pvRequest: TIocpAcceptExRequest);
begin
  FLocker.Enter;
  try
    FList.Remove(pvRequest);
  finally
    FLocker.Leave;
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
      FOwner.doError(lvErrCode);
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
  if FOwner = nil then
  begin
    FClientContext.Free;
    exit;
  end else if (FOwner.FDataMoniter <> nil) then
  begin
    FOwner.FDataMoniter.incResponseWSARecvCounter;
    FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
  end;

  if FBytesTransferred = 0 then
  begin      // no data recvd, socket is break
    FClientContext.Disconnect;
    FOwner.removeClientContext(FClientContext);
    FOwner.releaseClientContext(FClientContext);      
  end else
  begin
    try
      FClientContext.DoReceiveData;
    finally
      // post recv request
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
        FClientContext.Disconnect;
        FOwner.removeClientContext(FClientContext);
        FOwner.releaseClientContext(FClientContext);
      end;
    end;
  end;
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
      FOwner.doError(lvRet);

      // kick out clientContext
      FClientContext.Disconnect;
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

constructor TIocpSendRequest.Create;
begin
  inherited Create;
  FSending := false;
end;

destructor TIocpSendRequest.Destroy;
begin
  inherited Destroy;
end;

procedure TIocpSendRequest.HandleResponse;
begin
  if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
  begin
    FOwner.FDataMoniter.incSentSize(FBytesTransferred);
    FOwner.FDataMoniter.incResponseWSASendCounter;
  end;
  
  FClientContext.DoSendRequestCompleted;
  FSending := false;
end;

function TIocpSendRequest.postSendBufferRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvRet: Integer;
  dwFlag: Cardinal;
  wsaBuffer: TWsaBuf;
begin
  FSending := true;
  wsaBuffer.buf := buf;
  wsaBuffer.len := len;
  dwFlag := 0;
  FWokeBufferSize := 0;
  lvRet := WSASend(FClientContext.FRawSocket.SocketHandle,
                    @wsaBuffer,
                    1,
                    FWokeBufferSize,
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
       FSending := false;
       FOwner.doError(lvRet);
       
       /// kick out the clientContext
       FClientContext.Disconnect;
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

end.
