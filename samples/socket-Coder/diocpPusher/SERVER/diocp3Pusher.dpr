program diocp3Pusher;

uses
  FastMM4,
  FastMM4Messages,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDIOCPStreamCoder in '..\..\diocpCoders\uDIOCPStreamCoder.pas',
  uFMMonitor in '..\..\..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\..\..\Common\Frames\uRunTimeINfoTools.pas',
  uPullerMananger in 'Service\uPullerMananger.pas',
  uMapObject in 'Service\uMapObject.pas',
  uMyClientContext in 'Service\uMyClientContext.pas',
  SimpleMsgPack in '..\Common\SimpleMsgPack.pas',
  uZipTools in '..\..\diocpCoders\uZipTools.pas',
  uProjectProtocol in '..\Common\uProjectProtocol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
