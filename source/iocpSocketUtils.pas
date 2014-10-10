unit iocpSocketUtils;

interface

uses
  iocpWinsock2, Windows, SysUtils;

type
  TSocketState = (ssDisconnected, ssConnected, ssConnecting, ssListening, ssAccepting);


type
  TIocpAcceptEx = function(sListenSocket, sAcceptSocket: TSocket; lpOutputBuffer:
      Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength:
      DWORD; var lpdwBytesReceived: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;

  TIocpConnectEx = function(const s : TSocket; const name: PSOCKADDR; const
      namelen: Integer; lpSendBuffer : Pointer; dwSendDataLength : DWORD; var
      lpdwBytesSent : DWORD; lpOverlapped : LPWSAOVERLAPPED): BOOL; stdcall;


  //  Extention function "GetAcceptExSockAddrs"
  TIocpGetAcceptExSockAddrs = procedure(lpOutputBuffer: Pointer;
      dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
      var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer;
      var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); stdcall;

  TIocpDisconnectEx = function(const hSocket : TSocket; lpOverlapped: LPWSAOVERLAPPED;
     const dwFlags : DWORD; const dwReserved : DWORD) : BOOL; stdcall;

  
var
  IocpAcceptEx:TIocpAcceptEx;
  IocpConnectEx:TIocpConnectEx;
  IocpDisconnectEx:TIocpDisconnectEx;
  IocpGetAcceptExSockaddrs: TIocpGetAcceptExSockAddrs;

function getSocketAddr(pvAddr: string; pvPort: Integer): TSockAddrIn;
function socketBind(s: TSocket; const pvAddr: string; pvPort: Integer): Boolean;

implementation

const
  WINSOCK_LIB_VERSION : Word = $0202;

const
  WSAID_GETACCEPTEXSOCKADDRS: TGuid = (D1:$b5367df2;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));
  WSAID_ACCEPTEX: TGuid = (D1:$b5367df1;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));
  WSAID_CONNECTEX: TGuid = (D1:$25a207b9;D2:$ddf3;D3:$4660;D4:($8e,$e9,$76,$e5,$8c,$74,$06,$3e));
  {$EXTERNALSYM WSAID_DISCONNECTEX}
  WSAID_DISCONNECTEX: TGuid = (D1:$7fda2e11;D2:$8630;D3:$436f;D4:($a0,$31,$f5,$36,$a6,$ee,$c1,$57));

function creatTcpSocketHandle:THandle;
begin
  Result := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_IP, nil, 0, WSA_FLAG_OVERLAPPED);
  if (Result = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadAcceptEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_ACCEPTEX,
        SizeOf(WSAID_ACCEPTEX),
        @@IocpAcceptEx,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);

  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadDisconnectEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_DISCONNECTEX,
        SizeOf(WSAID_DISCONNECTEX),
        @@IocpDisconnectEx,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);

  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadAcceptExSockaddrs(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_GETACCEPTEXSOCKADDRS,
        SizeOf(WSAID_GETACCEPTEXSOCKADDRS),
        @@IocpGetAcceptExSockaddrs,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);



  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadConnecteEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_CONNECTEX,
        SizeOf(WSAID_CONNECTEX),
        @@IocpConnectEx,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);
  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure WSAStart;
var
  lvRET: Integer;
  WSData: TWSAData;
begin
  lvRET := WSAStartup($0202, WSData);
  if lvRET <> 0 then RaiseLastOSError;
end;

procedure loadExFunctions;
var
  skt:TSocket;
begin
  skt := creatTcpSocketHandle;
  LoadAcceptEx(skt);
  LoadConnecteEx(skt);
  LoadAcceptExSockaddrs(skt);
  LoadDisconnectEx(skt);
  closesocket(skt);
end;

function getSocketAddr(pvAddr: string; pvPort: Integer): TSockAddrIn;
begin
  Result.sin_family := AF_INET;
  Result.sin_addr.S_addr:= inet_addr(PAnsiChar(AnsiString(pvAddr)));
  Result.sin_port := htons(pvPort);
end;

function socketBind(s: TSocket; const pvAddr: string; pvPort: Integer): Boolean;
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
  Result := iocpWinsock2.bind(s, TSockAddr(sockaddr), SizeOf(sockaddr)) = 0;
end;


initialization
  WSAStart;
  loadExFunctions;

end.
