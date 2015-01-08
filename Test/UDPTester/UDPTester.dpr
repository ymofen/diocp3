program UDPTester;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  u_iocp_api in '..\iocp_api\u_iocp_api.pas',
  u_udp_iocp_api in 'u_udp_iocp_api.pas',
  Winsock2 in '..\UDPSERVERTester\Winsock2.pas',
  DRawWinSocket in 'DRawWinSocket.pas',
  DWinSocket2 in 'DWinSocket2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
