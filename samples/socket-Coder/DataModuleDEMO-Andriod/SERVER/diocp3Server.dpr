program diocp3Server;

uses
  FastMM4,
  FastMM4Messages,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDIOCPStreamCoder in '..\..\diocpCoders\uDIOCPStreamCoder.pas',
  uFMMonitor in '..\..\..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\..\..\Common\Frames\uRunTimeINfoTools.pas',
  uMyClientContext in 'Service\uMyClientContext.pas',
  udmMain in 'Service\udmMain.pas' {dmMain: TDataModule},
  SimpleMsgPack in '..\Common\SimpleMsgPack.pas',
  DZipTools in '..\..\diocpCoders\DZipTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
