unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Spin, StdCtrls, ExtCtrls, dwf;

type

  { TMainForm }

  TMainForm = class(TForm)
    RxPanel: TPanel;
    R3Label: TLabel;
    R4Label: TLabel;
    TrackBar1: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    hAd2 : HDWF;
    procedure SetupSPI;
  end;

var
  MainForm: TMainForm;

const
  Rwiper = 110.0; //
  Rab = 10000.0;  //

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.SetupSPI;
begin
  FDwfDigitalSpiFrequencySet( hAd2, 1E3 );     // 1kHz
  FDwfDigitalSpiClockSet( hAd2, DIO_1 );       // CLK = DIO_1
  FDwfDigitalSpiDataSet ( hAd2,  FUNC_DQ0_MOSI_SISO (* idxDQ *), DIO_2 );   // DQ0 (MOSI) = DIO_2
  FDwfDigitalSpiDataSet ( hAd2,  FUNC_DQ1_MISO (* idxD1 *), DIO_3 );        // DQ1 (MISO) = DIO_3
  FDwfDigitalSpiModeSet ( hAd2, 0 ); // SPI w 0 - Mode 0,0 , 3-Mode 1,1
  FDwfDigitalSpiOrderSet ( hAd2, 1 );  // MSB first
  FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_HIGH );   // /SEL = DIO_0, wymuś H
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    FDwfDeviceClose( self.hAd2 );
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
    msgErr : TDwfString512;
begin
    if not FDwfDeviceOpen( -1, @self.hAd2 ) then
    begin
         FDwfGetLastErrorMsg( msgErr );
         showMessage ( msgErr );
         Application.Terminate;
    end;
    // init SPI
    self. SetupSPI;
    self.TrackBar1Change( nil ); // na początek
end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
var
    spiTx : smallint;
    R3,R4 : double;
begin
  //wyliczenia
  R3 := Rab-(Rab*TrackBar1.Position)/255;
  R4 := Rab*TrackBar1.Position/255;
  self.R3Label.Caption := 'R3 = ' + FormatFloat( '0', R3 ) + 'Ω';
  self.R4Label.Caption := 'R4 = ' + FormatFloat( '0', R4 ) + 'Ω';
  self.RxPanel.Caption := 'Rx = R1*R4/R3 = ' + FormatFloat( '0', Rab*R4/R3 ) + 'Ω';
  // update potka
  spiTx := TrackBar1.Position and $00FF;
  FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_LOW );
  FDwfDigitalSpiWrite16 ( hAd2, DQ_MOSI_MISO , 16, @spiTx, 1 );
  FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_HIGH );
end;

end.

