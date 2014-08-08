program UnitTester;

uses
//  FastMM4,
//  FastMM4Messages,
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  ufrmDoublyLink in 'ufrmDoublyLink.pas' {frmDoublyLink},
  uThreadWorker in 'uThreadWorker.pas',
  DoublyLinked in 'DoublyLinked.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
 // Application.CreateForm(TfrmDoublyLink, frmDoublyLink);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
