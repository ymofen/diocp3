unit iocpReg;

interface

uses
  Classes;

procedure Register;

implementation

uses
  iocpTcpClient, iocpTcpServer;

procedure Register;
begin
  RegisterComponents('iocp vcl', [TIocpTcpClient, TIocpTcpServer]);
end;

end.
