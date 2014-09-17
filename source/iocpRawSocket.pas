unit iocpRawSocket;

interface

uses
  windows, SysUtils, iocpWinsock2;

const
  SIO_KEEPALIVE_VALS = IOC_IN or IOC_VENDOR or 4;

{ Other NT-specific options. }

  {$EXTERNALSYM SO_MAXDG}
  SO_MAXDG        = $7009;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_MAXPATHDG    = $700A;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  {$EXTERNALSYM SO_CONNECT_TIME}
  SO_CONNECT_TIME = $700C;

type
  TKeepAlive = record
    OnOff: Integer;
    KeepAliveTime: Integer;
    KeepAliveInterval: Integer;
  end;
  TTCP_KEEPALIVE = TKeepAlive;
  PTCP_KEEPALIVE = ^TKeepAlive;
  
  /// <summary>
  ///   raw socket object
  ///     thanks my friend(ryan)
  /// </summary>
  TRawSocket = class(TObject)
  private
    FSocketHandle: TSocket;
  public
    procedure close;    
    procedure createTcpSocket;

    /// <summary>
    ///   create socket handle for overlapped
    /// </summary>
    procedure createTcpOverlappedSocket;

    function bind(const pvAddr: string; pvPort: Integer): Boolean;
    function listen(const backlog: Integer = 0): Boolean;

    function Recv(var data; const len: Integer): Integer;
    function Send(const data; const len: Integer): Integer;

    function connect(const pvAddr: string; pvPort: Integer): Boolean;

    //zero if the time limit expired, or SOCKET_ERROR if an error occurred.
    function selectSocket(vReadReady, vWriteReady, vExceptFlag: PBoolean;
        pvTimeOut: Integer = 0): Integer;

    function setReadTimeOut(const pvTimeOut: Cardinal): Integer;

    /// <summary>
    ///   default 5000 check alive
    /// </summary>
    function setKeepAliveOption(pvKeepAliveTime: Integer = 5000): Boolean;


    /// <summary>
    ///   call in listen RawSocket instance
    /// </summary>
    function UpdateAcceptContext(pvSocket: TSocket): Boolean;

    function setNoDelayOption(pvOption:Boolean): Boolean;

    property SocketHandle: TSocket read FSocketHandle;
  end;

implementation

{ TRawSocket }

function TRawSocket.bind(const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    if pvAddr = '' then
    begin
      sin_addr.S_addr := inet_addr(PAnsichar(AnsiString('0.0.0.0')));
    end else
    begin
      sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    end;
    sin_port :=  htons(pvPort);
  end;
  Result := iocpWinsock2.bind(FSocketHandle, TSockAddr(sockaddr), SizeOf(sockaddr)) = 0;
end;

procedure TRawSocket.close;
var
  lvTempSocket: TSocket;
begin
  lvTempSocket := FSocketHandle;
  if lvTempSocket <> INVALID_SOCKET then
  begin
    FSocketHandle := INVALID_SOCKET;
    Windows.CancelIo(lvTempSocket);
    closesocket(lvTempSocket);
  end;
end;

function TRawSocket.connect(const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    sin_port :=  htons(pvPort);
  end;
  Result := iocpWinsock2.connect(FSocketHandle, TSockAddr(sockaddr), sizeof(TSockAddrIn))  = 0;
end;

procedure TRawSocket.createTcpOverlappedSocket;
begin
  FSocketHandle := WSASocket(AF_INET,SOCK_STREAM, IPPROTO_TCP, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure TRawSocket.createTcpSocket;
begin
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

function TRawSocket.listen(const backlog: Integer): Boolean;
var
  queueSize: Integer;
begin
  if backlog = 0 then
  begin
    queueSize := SOMAXCONN;
  end
  else begin
    queueSize := backlog;
  end;
  Result := iocpWinsock2.listen(FSocketHandle, queueSize) = 0;
end;

function TRawSocket.Recv(var data; const len: Integer): Integer;
begin
  Result := iocpWinsock2.recv(FSocketHandle, data, len, 0);
end;

function TRawSocket.selectSocket(vReadReady, vWriteReady, vExceptFlag:
    PBoolean; pvTimeOut: Integer = 0): Integer;
var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  if Assigned(vReadReady) then
  begin
    ReadFdsptr := @ReadFds;
    FD_ZERO(ReadFds);
    _FD_SET(FSocketHandle, ReadFds);
  end
  else
    ReadFdsptr := nil;
  if Assigned(vWriteReady) then
  begin
    WriteFdsptr := @WriteFds;
    FD_ZERO(WriteFds);
    _FD_SET(FSocketHandle, WriteFds);
  end
  else
    WriteFdsptr := nil;
  if Assigned(vExceptFlag) then
  begin
    ExceptFdsptr := @ExceptFds;
    FD_ZERO(ExceptFds);
    _FD_SET(FSocketHandle, ExceptFds);
  end
  else
    ExceptFdsptr := nil;
  if pvTimeOut >= 0 then
  begin
    tv.tv_sec := pvTimeOut div 1000;
    tv.tv_usec :=  1000 * (pvTimeOut mod 1000);
    Timeptr := @tv;
  end
  else
    Timeptr := nil;

  //The select function determines the status of one or more sockets, waiting if necessary,
  //to perform synchronous I/O.
  //  The select function returns the total number of socket handles that are ready
  //  and contained in the fd_set structures,
  //  zero if the time limit expired, or SOCKET_ERROR if an error occurred.
  //  If the return value is SOCKET_ERROR,
  //  WSAGetLastError can be used to retrieve a specific error code.
  
  Result := iocpWinsock2.select(FSocketHandle + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr);

  if Assigned(vReadReady) then
    vReadReady^ := FD_ISSET(FSocketHandle, ReadFds);
  if Assigned(vWriteReady) then
    vWriteReady^ := FD_ISSET(FSocketHandle, WriteFds);
  if Assigned(vExceptFlag) then
    vExceptFlag^ := FD_ISSET(FSocketHandle, ExceptFds);
end;

function TRawSocket.Send(const data; const len: Integer): Integer;
begin
  Result := iocpWinsock2.Send(FSocketHandle, data, len, 0);
end;

function TRawSocket.setKeepAliveOption(pvKeepAliveTime: Integer = 5000):
    Boolean;
var
  Opt, insize, outsize: integer;
  outByte: DWORD;
  inKeepAlive, outKeepAlive: TTCP_KEEPALIVE;
begin
  Result := false;
  Opt := 1;
  if SetSockopt(FSocketHandle, SOL_SOCKET, SO_KEEPALIVE,
     @Opt, sizeof(Opt)) = SOCKET_ERROR then exit;

  inKeepAlive.OnOff := 1;
  
  inKeepAlive.KeepAliveTime := pvKeepAliveTime;

  inKeepAlive.KeepAliveInterval := 1;
  insize := sizeof(TTCP_KEEPALIVE);
  outsize := sizeof(TTCP_KEEPALIVE);

  if WSAIoctl(FSocketHandle,
     SIO_KEEPALIVE_VALS,
     @inKeepAlive, insize,
     @outKeepAlive,
    outsize, outByte, nil, nil) <> SOCKET_ERROR then
  begin
    Result := true;
  end;
end;

function TRawSocket.setNoDelayOption(pvOption:Boolean): Boolean;
var
  bNoDelay: BOOL;
begin
  bNoDelay := pvOption;
  Result := setsockopt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay)) <> SOCKET_ERROR;
end;

function TRawSocket.setReadTimeOut(const pvTimeOut: Cardinal): Integer;
begin
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
end;

function TRawSocket.UpdateAcceptContext(pvSocket: TSocket): Boolean;
begin
  result := setsockopt(pvSocket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
    PAnsiChar(@FSocketHandle),
   SizeOf(TSocket)) <> SOCKET_ERROR
end;

end.
