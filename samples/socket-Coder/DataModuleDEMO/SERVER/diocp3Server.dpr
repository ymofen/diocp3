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
  pcre in '..\..\..\qdac3-source\pcre.pas',
  PerlRegEx in '..\..\..\qdac3-source\PerlRegEx.pas',
  qmsgpack in '..\..\..\qdac3-source\qmsgpack.pas',
  qrbtree in '..\..\..\qdac3-source\qrbtree.pas',
  qstring in '..\..\..\qdac3-source\qstring.pas',
  qworker in '..\..\..\qdac3-source\qworker.pas',
  uZipTools in '..\..\..\Common\uZipTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmMain, dmMain);
  Application.Run;
end.
