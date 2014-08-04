unit iocpReg;

interface

uses
  Classes;

procedure Register;

implementation

uses
  iocpTcpClient, iocpTcpServer, uIOCPCentre;

procedure Register;
begin
  RegisterComponents('iocp vcl', [TIocpTcpClient, TIocpTcpServer, TIOCPConsole]);
end;

end.
