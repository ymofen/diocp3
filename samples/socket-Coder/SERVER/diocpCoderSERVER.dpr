program diocpCoderSERVER;

uses
  FastMM4,
  FastMM4Messages,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uFMMonitor in '..\..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\..\Common\Frames\uRunTimeINfoTools.pas',
  JSonStream in '..\diocpCoders\JSonStream.pas',
  uIOCPJSonStreamDecoder in '..\diocpCoders\uIOCPJSonStreamDecoder.pas',
  uIOCPJSonStreamEncoder in '..\diocpCoders\uIOCPJSonStreamEncoder.pas',
  superobject in '..\diocpCoders\superobject.pas',
  uCRCTools in '..\diocpCoders\uCRCTools.pas',
  uZipTools in '..\diocpCoders\uZipTools.pas',
  uMyTypes in '..\diocpCoders\uMyTypes.pas',
  AnsiStringTools in '..\diocpCoders\AnsiStringTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
