(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v0.1(2014-7-16 21:36:30)
 *     + first release
 *   v0.2
 *     + add sending queue
 *)
unit iocpTcpClient;

interface

uses
  WinSock, SysUtils, iocpEngine, iocpWinsock2, iocpProtocol, iocpSocketUtils,
  Windows, Classes, iocpRawSocket, BaseQueue;

const
  buf_size = 1024 * 50;

type
  TIocpTcpClient = class;

  TOnConnected = procedure(Sender:TObject; errCode:Integer) of object;


  TOnWorkError = procedure(Sender:TObject; errCode:Integer) of object;

  TOnDataRecvd = procedure(Sender:TObject; buf:Pointer; len:cardinal; errCode:Integer) of object;

  /// <summary>
  ///   on post request is completed
  /// </summary>
  TOnDataRequestCompleted = procedure(pvRequest: TIocpRequest) of object;

  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FBuffer: iocpWinsock2.TWsaBuf;
    FRecvdBufferSize: Cardinal;
    FRecvdFlag: Cardinal;
    FOwner: TIocpTcpClient;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;
    /// <summary>
    ///   post recv request to iocp queue
    /// </summary>
    function PostRequest: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;



  /// <summary>
  ///   WSASend io request
  /// </summary>
  TIocpSendRequest = class(TIocpRequest)
  private
    FIsBusying:Boolean;

    FAlive: Boolean;

    FBytesSize:Cardinal;

    FCopyBuf:TWsaBuf;

    // send buf record
    FWSABuf:TWsaBuf;


    FBuf:Pointer;
    FPosition:Cardinal;
    FLen:Cardinal;

    FOwner: TIocpTcpClient;

    FOnDataRequestCompleted: TOnDataRequestCompleted;
    /// <summary>
    ///   checkStart to post
    /// </summary>
    function checkStart: Boolean;

  protected
    /// <summary>
    ///   is all buf send completed?
    /// </summary>
    function isCompleted:Boolean;virtual;

    /// <summary>
    ///  on request successful
    /// </summary>
    procedure onSendRequestSucc; virtual;

    /// <summary>
    ///   post send a block
    /// </summary>
    function checkSendNextBlock: Boolean; virtual;
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


  public
    constructor Create; virtual;

    destructor Destroy; override;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure setBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true);

    property Owner: TIocpTcpClient read FOwner;
    /// <summary>
    ///   on entire buf send completed
    /// </summary>
    property OnDataRequestCompleted: TOnDataRequestCompleted read
        FOnDataRequestCompleted write FOnDataRequestCompleted;
  published
  end;

  TIocpSendRequestClass = class of TIocpSendRequest;

  /// <summary>
  ///   connectEx io request
  /// </summary>
  TIocpConnectExRequest = class(TIocpRequest)
  private
    FBytesSent: DWORD;
    FOwner: TIocpTcpClient;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;

    /// <summary>
    ///   post connectEx request to iocp queue
    /// </summary>
    function PostRequest: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;



  TIocpTcpClient = class(TComponent)
  private
    FSocketState:TSocketState;

    FSending:Boolean;

    /// sendRequest link
    FSendRequestLink: TIocpRequestSingleLink;
    
    FcurrSendRequest:TIocpSendRequest;


    FConnectExRequest: TIocpConnectExRequest;
    FIocpSendRequest: TIocpSendRequest;
    FHost: String;
    FRawSocket: TRawSocket;
    FIOCPEngine: TIocpEngine;
    FOnConnected: TOnConnected;
    FOnDataRecvd: TOnDataRecvd;
    FOnDisconnected: TNotifyEvent;
    FOnError: TOnWorkError;
    FonSocketStateChanged: TNotifyEvent;
    FPort: Integer;
    FRecvRequest: TIocpRecvRequest;

    // sendRequest pool
    FSendRequestPool: TBaseQueue;
    FWSARecvBufferSize: Integer;
    FWSASendBufferSize: Integer;
    procedure SetWSARecvBufferSize(const Value: Integer);
    procedure SetWSASendBufferSize(const Value: Integer);
  protected
    FIocpSendRequestClass:TIocpSendRequestClass;
  protected



    /// <summary>
    ///   post next sendRequest
    /// </summary>
    procedure checkNextSendRequest;
    /// <example>
    ///  sendRequest to pool
    /// </example>
    procedure checkReleaseRes;
    /// <summary>
    ///   on recved data, run in iocp worker thread
    /// </summary>
    procedure DoRecvd(buf: Pointer; len: Cardinal; errCode: Integer); virtual;

    /// <summary>
    ///   on connected request response, run in iocp worker thread
    /// </summary>
    procedure DoConnected();

    /// <summary>
    ///   set socket current state;
    /// </summary>
    procedure setSocketState(pvState:TSocketState);

    procedure doError(pvErrorCode:Integer);
    /// <summary>
    ///   pop sendRequest object
    /// </summary>
    function getSendRequest: TIocpSendRequest;
    function GetSocketState: TSocketState;
    /// <summary>
    ///   post reqeust to sending queue,
    ///    fail, push back to pool
    /// </summary>
    function postSendRequest(pvSendRequest:TIocpSendRequest): Boolean;
    /// <summary>
    ///   push back to pool
    /// </summary>
    function releaseSendRequest(pvObject:TIocpSendRequest): Boolean;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    function isActive:Boolean;

    /// <summary>
    ///    sync execute connect
    ///      ssDisconnected -> ssConnected/ssDisconnected
    /// </summary>
    procedure Connect;

    /// <summary>
    ///   async execute connect
    ///     ssDisconnected -> ssConnecting -> ssConnected/ssDisconnected
    /// </summary>
    procedure connectASync();


    /// <summary>
    ///   async send buffer
    ///     send to iocp queue
    /// </summary>
    function sendBufferAsync(pvBuffer:Pointer; pvLen:Cardinal): Boolean;

    procedure Disconnect;

  published

    property Host: String read FHost write FHost;


    property Port: Integer read FPort write FPort;

    property OnDisconnected: TNotifyEvent read FOnDisconnected write
        FOnDisconnected;

    /// <summary>
    ///   socket current state
    /// </summary>
    property SocketState: TSocketState read GetSocketState;
    /// <summary>
    ///   post wsaRecv request block size
    /// </summary>
    property WSARecvBufferSize: Integer read FWSARecvBufferSize write
        SetWSARecvBufferSize;
    /// <summary>
    ///   max size for post WSASend
    /// </summary>
    property WSASendBufferSize: Integer read FWSASendBufferSize write
        SetWSASendBufferSize;


    /// <summary>
    ///  on state changed
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property onSocketStateChanged: TNotifyEvent read FonSocketStateChanged write
        FonSocketStateChanged;
    

    /// <summary>
    ///   invoke in iocp worker thread
    /// </summary>
    property OnConnected: TOnConnected read FOnConnected write FOnConnected;

    /// <summary>
    ///  data recv
    ///   invoke in iocp worker thread
    /// </summary>
    property OnDataRecvd: TOnDataRecvd read FOnDataRecvd write FOnDataRecvd;


    /// <summary>
    ///  on work error
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property OnError: TOnWorkError read FOnError write FOnError;



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

procedure TIocpTcpClient.Disconnect;
begin
  if not isActive then Exit;
  FRawSocket.close;
  setSocketState(ssDisconnected);

  if Assigned(FOnDisconnected) then
  begin
    FOnDisconnected(Self);
  end;
end;

procedure TIocpTcpClient.connectASync;
begin
  if isActive then Exit;
  
  if not FIOCPEngine.Active then FIOCPEngine.start;
  
  FRawSocket.createTcpOverlappedSocket;

  
  FIOCPEngine.IocpCore.bind2IOCPHandle(FRawSocket.SocketHandle, 0);
  
  FConnectExRequest.PostRequest;
end;

constructor TIocpTcpClient.Create(AOwner:TComponent);
begin
  inherited;
  FWSARecvBufferSize := 1024 * 4;

  FWSASendBufferSize := 1024 * 4;

  FSendRequestPool := TBaseQueue.Create;

  FSendRequestLink := TIocpRequestSingleLink.Create(10);
  
  FIOCPEngine := TIocpEngine.Create();
  FIOCPEngine.setWorkerCount(1);

  FRawSocket := TRawSocket.Create();

  FRecvRequest := TIocpRecvRequest.Create();
  FRecvRequest.FOverlapped.iocpRequest := FRecvRequest;
  FRecvRequest.FOwner := Self;

  FConnectExRequest := TIocpConnectExRequest.Create();
  FConnectExRequest.FOverlapped.iocpRequest := FConnectExRequest;
  FConnectExRequest.FOwner := Self;

  FIocpSendRequest := TIocpSendRequest.Create();
  FIocpSendRequest.FOverlapped.iocpRequest := FIocpSendRequest;
  FIocpSendRequest.FOwner := Self;                            
end;

destructor TIocpTcpClient.Destroy;
begin

  // disable repeat request
  setSocketState(ssDisconnected);

  FRawSocket.close;
  FIOCPEngine.safeStop;

  checkReleaseRes;
  Assert(FSendRequestLink.Count = 0);
  FSendRequestLink.Free;


  FSendRequestPool.FreeDataObject;
  FSendRequestPool.Free;


  FRecvRequest.Free;
  FConnectExRequest.Free;
  FIocpSendRequest.Free;

  FRawSocket.Free;
  FIOCPEngine.Free;
  inherited Destroy;
end;

procedure TIocpTcpClient.checkNextSendRequest;
var
  lvRequest:TIocpSendRequest;
begin
  lvRequest := TIocpSendRequest(FSendRequestLink.Pop);
  if lvRequest <> nil then
  begin
    FcurrSendRequest := lvRequest;
    if lvRequest.checkStart then
    begin
      ;
    end else
    begin
      {$IFDEF DEBUG_MSG_ON}
         logDebugMessage('TIocpTcpClient.checkNextSendRequest.checkStart return false',  []);
      {$ENDIF}

      /// kick out the clientContext
      Disconnect;
    end;
  end else
  begin  // no request to send
    if lock_cmp_exchange(True, false, FSending) = False then
    begin
      FSending := FSending;
    end;
  end;
end;

procedure TIocpTcpClient.checkReleaseRes;
var
  lvRequest:TIocpSendRequest;
begin
  while true do
  begin
    lvRequest :=TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest <> nil then
    begin
      releaseSendRequest(lvRequest);
    end else
    begin
      Break;
    end;
  end;
end;

procedure TIocpTcpClient.DoConnected;
begin

end;

procedure TIocpTcpClient.doError(pvErrorCode: Integer);
begin
  if Assigned(FOnError) then
    FOnError(self, pvErrorCode);
end;

procedure TIocpTcpClient.DoRecvd(buf: Pointer; len: Cardinal; errCode: Integer);
begin
  if Assigned(FOnDataRecvd) then
    FOnDataRecvd(Self, buf,len, errCode);;


end;

function TIocpTcpClient.isActive: Boolean;
begin
  Result := (FSocketState = ssConnected);  
end;

procedure TIocpTcpClient.Connect;
var
  lvAddr:TSockAddrIn;
  lvRet:Integer;
begin
  if isActive then Exit;
  
  if not FIOCPEngine.Active then FIOCPEngine.start;

  FRawSocket.createTcpOverlappedSocket;

  if not FRawSocket.bind('0.0.0.0', 0) then
  begin
     RaiseLastOSError;
  end;

  FIOCPEngine.IocpCore.bind2IOCPHandle(FRawSocket.SocketHandle, 0);

  lvAddr := getSocketAddr(FHost, FPort);
  lvRet := iocpWinsock2.connect(FRawSocket.SocketHandle, TSockAddr(lvAddr), sizeof(TSockAddrIn));
  if lvRet = SOCKET_ERROR then
    RaiseLastOSError;

  FSocketState := ssConnected;

  DoConnected;

  // post recv quest
  FRecvRequest.PostRequest; 
end;

function TIocpTcpClient.getSendRequest: TIocpSendRequest;
begin
  Result := TIocpSendRequest(FSendRequestPool.Pop);
  if Result = nil then
  begin
    if FIocpSendRequestClass <> nil then
    begin
      Result := FIocpSendRequestClass.Create;
    end else
    begin
      Result := TIocpSendRequest.Create;
    end;
  end;
  Result.FAlive := true;
  Result.DoCleanup;
  Result.FOwner := Self;
end;

function TIocpTcpClient.GetSocketState: TSocketState;
begin
  Result := FSocketState;
end;

function TIocpTcpClient.postSendRequest(pvSendRequest: TIocpSendRequest):
    Boolean;
begin
  if FSendRequestLink.Push(pvSendRequest) then
  begin
    Result := true;

    if lock_cmp_exchange(False, True, FSending) = False then
    begin
      // start sending
      checkNextSendRequest;
    end;
  end else
  begin
    {$IFDEF DEBUG_MSG_ON}
      logDebugMessage('Push sendRequest to Sending Queue fail, queue size:%d',
         [FSendRequestLink.Count]);
    {$ENDIF}

    releaseSendRequest(pvSendRequest);
    
    Disconnect;
    Result := false;
  end;
end;

function TIocpTcpClient.releaseSendRequest(pvObject:TIocpSendRequest): Boolean;
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

function TIocpTcpClient.sendBufferAsync(pvBuffer:Pointer; pvLen:Cardinal):
    Boolean;
var
  lvRequest:TIocpSendRequest;
begin            
  lvRequest := getSendRequest;
  lvRequest.setBuffer(pvBuffer, pvLen);
  Result := postSendRequest(lvRequest);

  //Result := FIocpSendRequest(pvBuffer, pvLen);
end;

procedure TIocpTcpClient.setSocketState(pvState: TSocketState);
begin
  FSocketState := pvState;
  if Assigned(FonSocketStateChanged) then
    FonSocketStateChanged(Self);  
end;

procedure TIocpTcpClient.SetWSARecvBufferSize(const Value: Integer);
begin
  FWSARecvBufferSize := Value;
  if FWSARecvBufferSize = 0 then
  begin
    FWSARecvBufferSize := 1024 * 4;
  end;
end;

procedure TIocpTcpClient.SetWSASendBufferSize(const Value: Integer);
begin
  FWSASendBufferSize := Value;
  if FWSASendBufferSize <=0 then
    FWSASendBufferSize := 1024 * 4;
end;

constructor TIocpRecvRequest.Create;
begin
  inherited Create;
  GetMem(FBuffer.buf, buf_size);
  FBuffer.len := buf_size;
end;

destructor TIocpRecvRequest.Destroy;
begin
  FreeMem(FBuffer.buf, buf_size);
  inherited Destroy;
end;

procedure TIocpRecvRequest.HandleResponse;
begin
  if FBytesTransferred = 0 then
  begin      // no data recvd, socket is break
    FOwner.Disconnect;
  end else
  begin
    try
      FOwner.DoRecvd(FBuffer.buf, FBytesTransferred, FErrorCode);
    finally
      // post recv request
      FOwner.FRecvRequest.PostRequest;
    end;

  end;
end;

function TIocpRecvRequest.PostRequest: Boolean;
var
  lvRet:Integer;
begin
  FRecvdBufferSize := 0;
  FRecvdFlag := 0;
  FBuffer.len := buf_size;

  lvRet := iocpWinsock2.WSARecv(FOwner.FRawSocket.SocketHandle,
     @FBuffer,
     1,
     FRecvdBufferSize,
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
    end;
  end else
  begin
    Result := True;
  end;
end;

constructor TIocpConnectExRequest.Create;
begin
  inherited Create;
end;

destructor TIocpConnectExRequest.Destroy;
begin
  inherited Destroy;
end;

procedure TIocpConnectExRequest.HandleResponse;
begin
  if FErrorCode = 0 then
  begin
    FOwner.setSocketState(ssConnected);

    // post recv request, start to recv data
    FOwner.FRecvRequest.PostRequest;
  end else
  begin
    FOwner.setSocketState(ssDisconnected);

    // trigger error event
    FOwner.doError(FErrorCode);
  end;

  if Assigned(FOwner.FOnConnected) then
    FOwner.FOnConnected(Self, FErrorCode);
end;

function TIocpConnectExRequest.PostRequest: Boolean;
var
  lvSockAddrIn:TSockAddrIn;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:Pointer;
begin
  FOwner.setSocketState(ssConnecting);
  lvSockAddrIn := iocpSocketUtils.getSocketAddr(FOwner.FHost, FOwner.FPort);

  iocpSocketUtils.socketBind(FOwner.FRawSocket.SocketHandle, '0.0.0.0', 0);

  lp :=@FOverlapped;
  lvRet := iocpSocketUtils.IocpConnectEx(FOwner.FRawSocket.SocketHandle,
        @lvSockAddrIn
        , sizeOf(lvSockAddrIn)
        , nil
        , 0
        , FBytesSent
        , lp
        );
  if not lvRet then
  begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then
    begin
      FOwner.doError(lvErrCode);
      FOwner.setSocketState(ssDisconnected);
    end;
  end else
  begin
    Result := True;
  end;


end;

function TIocpSendRequest.checkSendNextBlock: Boolean;
var
  l:Cardinal;
begin
  if FPosition < FLen then
  begin
    l := FLen - FPosition;
    if l > FOwner.FWSASendBufferSize then l := FOwner.FWSASendBufferSize;

    Result := InnerPostRequest(Pointer(IntPtr(FBuf) + IntPtr(FPosition)), l)

  end else
  begin
    Result := false;
  end;
end;

constructor TIocpSendRequest.Create;
begin
  inherited Create;

end;

destructor TIocpSendRequest.Destroy;
begin
  if FCopyBuf.len <> 0 then
  begin
    FreeMem(FCopyBuf.buf);
  end;
  inherited Destroy;
end;

procedure TIocpSendRequest.DoCleanUp;
begin
  FBytesSize := 0;
  FOwner := nil;
  FBuf := nil;
  FLen := 0;
  FPosition := 0;
end;

procedure TIocpSendRequest.HandleResponse;
begin
  FIsBusying := false;
  if FOwner = nil then
  begin
    Self.Free;
    exit;
  end;

  if FErrorCode <> 0 then
  begin
    {$IFDEF DEBUG_MSG_ON}
      logDebugMessage('TIocpSendRequest.HandleResponse FErrorCode:%d',  [FErrorCode]);
    {$ENDIF}
    
    FOwner.Disconnect;

    // release request
    FOwner.releaseSendRequest(Self);
  end else
  begin
    onSendRequestSucc;
    if isCompleted then    // is all buf send completed?
    begin
      FOwner.checkNextSendRequest;

      if Assigned(FOnDataRequestCompleted) then
      begin
        FOnDataRequestCompleted(Self);
      end;

      // release request
      FOwner.releaseSendRequest(Self);
    end else
    begin
      if not checkSendNextBlock then
      begin
        {$IFDEF DEBUG_MSG_ON}
           logDebugMessage('TIocpSendRequest.checkSendNextBlock return false',  []);
        {$ENDIF}

        /// kick out the clientContext
        FOwner.Disconnect;
      end;
    end;
  end;
end;

function TIocpSendRequest.InnerPostRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvRet: Integer;
  dwFlag: Cardinal;
  lpNumberOfBytesSent:Cardinal;
begin
  FIsBusying := True;
  FBytesSize := len;
  FWSABuf.buf := buf;
  FWSABuf.len := len;
  dwFlag := 0;
  lpNumberOfBytesSent := 0;
  lvRet := WSASend(FOwner.FRawSocket.SocketHandle,
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
       FIsBusying := False;
    {$IFDEF DEBUG_MSG_ON}
       logDebugMessage('TIocpSendRequest.InnerPostRequest Error:%d',  [lvRet]);
    {$ENDIF}

       FOwner.doError(lvRet);

       /// kick out the clientContext
       FOwner.Disconnect;

       FOwner.releaseSendRequest(Self);
    end;
  end else
  begin
    Result := True;
  end;
end;

function TIocpSendRequest.isCompleted: Boolean;
begin
  Result := FPosition >= FLen;
end;

procedure TIocpSendRequest.onSendRequestSucc;
begin
  FPosition := FPosition + self.FBytesTransferred;
end;

procedure TIocpSendRequest.setBuffer(buf: Pointer; len: Cardinal; pvCopyBuf:
    Boolean = true);
begin
  if pvCopyBuf then
  begin
    if FCopyBuf.len > 0 then FreeMem(FCopyBuf.buf);

    FCopyBuf.len := len;
    GetMem(FCopyBuf.buf, FCopyBuf.len);
    Move(buf^, FCopyBuf.buf^, FCopyBuf.len);
    FBuf := FCopyBuf.buf;
    FLen := FCopyBuf.len;
  end else
  begin
    FBuf := buf;
    FLen := len;
  end;
  FPosition := 0;
end;

function TIocpSendRequest.checkStart: Boolean;
begin
  Result := checkSendNextBlock;
end;


end.
