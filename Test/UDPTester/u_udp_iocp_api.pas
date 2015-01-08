unit u_udp_iocp_api;

interface

uses
  u_iocp_api, Winsock2, SysUtils, Windows;

type
  udp_recv_request = packed record
    _overlapped: OVERLAPPEDEx;
    _SockAddrIn: TSockAddrIn;
    request:io_request;
    innerBuf: array[0..1023] of Byte;
    buf: WSABUF;

    function PostWSARecv(s: TSocket): Boolean;

  
  end;



implementation

function udp_recv_request.postWSARecv(s: TSocket): Boolean;
var
  lvRet, lvDNACounter:Integer;
  lpNumberOfBytesRecvd: Cardinal;
  lvRecvFlag:Cardinal;
  lvFromlen:Cardinal;
begin
  buf.buf := PAnsiChar(@innerBuf[0]);
  buf.len := 1024;

  _SockAddrIn.sin_family := AF_INET;
  _SockAddrIn.sin_addr.S_addr := 0;
  _SockAddrIn.sin_port := htons(0);
  lvFromlen := SizeOf(_SockAddrIn);

  lvRet := WSARecvFrom(s,
    @buf,
    1,
    lpNumberOfBytesRecvd,
    lvRecvFlag,
    @_SockAddrIn, @lvFromlen,
    LPWSAOVERLAPPED(@_overlapped), nil);
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
      raiseLastOSError;
    end;
  end else
  begin
    Result := true;
  end;
end;



end.
