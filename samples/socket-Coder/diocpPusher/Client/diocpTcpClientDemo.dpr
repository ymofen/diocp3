program diocpTcpClientDemo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDIOCPStreamCoder in '..\..\diocpCoders\uDIOCPStreamCoder.pas',
  RawTcpClient in '..\..\..\..\source\RawTcpClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
