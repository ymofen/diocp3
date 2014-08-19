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
  qrbtree in '..\QTools\qrbtree.pas',
  qstring in '..\QTools\qstring.pas',
  qworker in '..\QTools\qworker.pas',
  uPackageObject in '..\Common\uPackageObject.pas',
  SimpleMsgPack in '..\Common\SimpleMsgPack.pas',
  udmMain in 'Service\udmMain.pas' {dmMain: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmMain, dmMain);
  Application.Run;
end.
