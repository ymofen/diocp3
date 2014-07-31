program UnitTester;

uses
  FastMM4,
  FastMM4Messages,
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  ufrmDoublyLink in 'ufrmDoublyLink.pas' {frmDoublyLink},
  uThreadWorker in 'uThreadWorker.pas',
  iocpUILogger in 'iocpUILogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
 // Application.CreateForm(TfrmDoublyLink, frmDoublyLink);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
