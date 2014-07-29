program iocpTaskDemo;

uses
  FastMM4,
  FastMM4Messages,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  iocpTask in '..\..\source\iocp-task\iocpTask.pas',
  BaseQueue in '..\..\source\utils\BaseQueue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
