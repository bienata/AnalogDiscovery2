unit main;

{$mode objfpc}{$H+}

interface

uses
  cthreads, cmem, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, StdCtrls, dwf;

type
  TAD2Thread = class( TThread )
    private
    protected
      procedure Execute; override;
    public
      hAd2 : HDWF;
      currentOutVoltage : double;
      requiredOutVoltage : double;
      Constructor Create( CreateSuspended : boolean );
      procedure SetupSPI;
      procedure SetupAnalogIn;
      procedure UpdateVoltageSettings;
  end;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    TrackBar1: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ad2thread : TAD2Thread;
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     ad2thread.Terminate;
     ad2thread.WaitFor;
     FreeAndNil( ad2thread );
     writeln ('stop: '  + FormatDateTime('', Now ));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     writeln ('start: ' + FormatDateTime('', Now ));
     ad2thread := TAD2Thread.Create( false );
end;

constructor TAD2Thread.Create( CreateSuspended : boolean );
begin
     inherited Create( CreateSuspended );
     FreeOnTerminate := false;
end;

procedure TAD2Thread.SetupSPI;
begin
     FDwfDigitalSpiFrequencySet( hAd2, 1E3 );     // 1kHz
     FDwfDigitalSpiClockSet( hAd2, DIO_1 );       // CLK = DIO_1
     FDwfDigitalSpiDataSet ( hAd2,  FUNC_DQ0_MOSI_SISO (* idxDQ *), DIO_2 );   // DQ0 (MOSI) = DIO_2
     FDwfDigitalSpiDataSet ( hAd2,  FUNC_DQ1_MISO (* idxD1 *), DIO_3 );        // DQ1 (MISO) = DIO_3
     FDwfDigitalSpiModeSet ( hAd2, 0 ); // SPI w 0 - Mode 0,0 , 3-Mode 1,1
     FDwfDigitalSpiOrderSet ( hAd2, 1 );  // MSB first
     FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_HIGH );   // /SEL = DIO_0, wymuś H
end;

procedure TAD2Thread.SetupAnalogIn;
begin
     FDwfAnalogInChannelEnableSet( hAd2, ANALOGIN_0, true );
     FDwfAnalogInChannelOffsetSet( hAd2, ANALOGIN_0, 0.0 );
     FDwfAnalogInChannelRangeSet( hAd2 , ANALOGIN_0, 50 ); // +-25V
     FDwfAnalogInConfigure( hAd2, false, false );
end;

procedure TAD2Thread.UpdateVoltageSettings;
begin
     self.requiredOutVoltage := double( Form1.TrackBar1.Position );
     Form1.Edit1.Text := FormatFloat( '000.0', currentOutVoltage );
end;

procedure TAD2Thread.Execute; // nadpisana!
var
   msgErr : TDwfString512;
   potValue : smallint;
   vErr : double;
   n : integer;
   adc : double;
const
  SAMPLES : integer = 50;
  epsi : double = 0.05;
begin
     requiredOutVoltage := 5.00;

     writeln ('ad2 thread start');

     if not FDwfDeviceOpen( -1, @self.hAd2 ) then
     begin
           FDwfGetLastErrorMsg( msgErr );
           writeln ( msgErr );
           exit;
      end;
     self.SetupSPI;
     self.SetupAnalogIn;
     potValue := 0;
     repeat

       // average AIN
       currentOutVoltage := 0;
       for n := 1 to SAMPLES do
       begin
              FDwfAnalogInStatus( hAd2, false, nil );
              FDwfAnalogInStatusSample( hAd2, ANALOGIN_0, @adc );
              currentOutVoltage := currentOutVoltage + adc;
       end;
       currentOutVoltage := currentOutVoltage / SAMPLES;

       vErr := requiredOutVoltage - currentOutVoltage;
       if abs(vErr) > epsi then
       begin
         if vErr > 0 then
         begin
              Inc (potValue, 5 );
              if potValue > $FF then potValue := $FF;
         end
         else
         begin
            Dec (potValue, 5 );
            if potValue < 0 then potValue := 0;
         end;
       end;

       FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_LOW );
       FDwfDigitalSpiWrite16 ( hAd2, DQ_MOSI_MISO , 16, @potValue, 1 );
       FDwfDigitalSpiSelect ( hAd2, DIO_0, SPI_SEL_HIGH );

       writeln ( ' Vset=' + FormatFloat( '000.0', requiredOutVoltage ) + ' ' +
                 ' Vout=' + FormatFloat( '000.0', currentOutVoltage )  + ' ' +
                 '  err=' + FormatFloat( '000.0', vErr ) + ' ' +
                 '  pot=' + IntToStr( potValue ) );

       synchronize( @UpdateVoltageSettings );

       if Terminated then break;
       self.Sleep( 10 );
     until false;

     FDwfDeviceClose( self.hAd2 );

     writeln ('ad2 thread stop');
end;


end.

