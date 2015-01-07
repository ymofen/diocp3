unit u_udp_iocp_api;

interface

uses
  u_iocp_api, Winsock2;

type
  udp_recv_request = packed record
    overlappedEx: OVERLAPPEDEx;
    request:io_request;
    innerBuf: array[0..1023] of Byte;
    buf: WSABUF;

    function postWSARecv(s: TSocket): Integer;
  end;



implementation

function udp_recv_request.postWSARecv(s: TSocket): Integer;
var
  lvRet, lvDNACounter:Integer;
  lpNumberOfBytesRecvd: Cardinal;
  lvRecvFlag:Cardinal;
  lvsaRemote: TSockAddrIn;
  lvFromlen:Cardinal;
begin
  buf.buf := PAnsiChar(@innerBuf[0]);
  buf.len := 1024;

  lvsaRemote.sin_family := AF_INET;
  lvsaRemote.sin_addr.S_addr := 0;
  lvsaRemote.sin_port := htons(0);
  lvFromlen := SizeOf(lvsaRemote);

  lvRet := WSARecvFrom(s,
    @buf,
    1,
    lpNumberOfBytesRecvd,
    lvRecvFlag,
    @lvsaRemote, @lvFromlen,
    LPWSAOVERLAPPED(@overlappedEx), nil);
  if lvRet <> WSA_IO_PENDING then
  begin

  end;
  Result := lvRet;
end;



end.
