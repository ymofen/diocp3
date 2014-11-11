program diocpTcpClientDemo;

uses
  FastMM4,
  ExceptionLog,
  FastMM4Messages,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  iocpBaseSocket in '..\..\..\source\iocpBaseSocket.pas',
  iocpClientSocket in '..\..\..\source\iocpClientSocket.pas',
  uFMMonitor in 'Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in 'Frames\uRunTimeINfoTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
