program iocp_api_tester;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  u_iocp_api in 'u_iocp_api.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
