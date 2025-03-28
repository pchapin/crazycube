--------------------------------------------------------------------------------
-- FILE   : motors-messages.adb
-- SUBJECT: Body of a package that implements the main part of the motors module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with Name_Resolver;
with motors.API;  -- Needed so that the types in the API can be used here.
with CubedOS.Log_Server.API;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body motors.Messages is
   use Message_Manager;
   Motor_One : Float := 3.0;
   Motor_Two : Float := 3.0;
   Motor_Three : Float := 3.0;
   Motor_Four : Float := 3.0;
   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Increase_Voltage_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Increase_Voltage(Message)
   is
      Status  : Message_Status_Type;
      Voltage_One : Float;
      Voltage_Two : Float;
      Voltage_Three : Float;
      Voltage_Four: Float;
      Time : Duration;
      Move_Reply : Message_Record;
      Successful : Boolean := true;
   begin
      Motors.API.Increase_Voltage_Decode
        (Message       => Message,
         VoltageOne    => Voltage_One,
         VoltageTwo    => Voltage_Two,
         VoltageThree  => Voltage_Three,
         VoltageFour   => Voltage_Four,
         Time          => Time,
         Decode_Status => Status);
      -- moves the drone
      if (Motor_One + Voltage_One <= 4.2) and (Motor_Two + Voltage_Two <= 4.2) and (Motor_Three + Voltage_Three <= 4.2) and (Motor_Four + Voltage_Four <= 4.2) then
         Motor_One := Motor_One + Voltage_One;
         Motor_Two := Motor_Two + Voltage_Two;
         Motor_Three := Motor_Three + Voltage_Three;
         Motor_Four := Motor_Four + Voltage_Four;
         delay Time;
         Motor_One := Motor_One - Voltage_One;
         Motor_Two := Motor_Two - Voltage_Two;
         Motor_Three := Motor_Three - Voltage_Three;
         Motor_Four := Motor_Four - Voltage_Four;
      else
         Successful := False;
      end if;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Name_Resolver.Controller,
         Request_ID       => 1,
         Successful          => Successful);
      Route_Message (Message => Move_Reply);
      Put_Line("successful. voltage_one = ");
         Put(Voltage_One, Fore => 0, Aft => 3, Exp => 0);
      Put_Line("reply has been sent. motor_one = ");
      Put(Motor_One, Fore => 0, Aft => 3, Exp => 0);
   end Handle_Increase_Voltage_Request;

   procedure Handle_Decrease_Voltage_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Decrease_Voltage(Message)
   is
      Status  : Message_Status_Type;
      Voltage_One : Float;
      Voltage_Two : Float;
      Voltage_Three : Float;
      Voltage_Four: Float;
      Time : Duration;
      Move_Reply : Message_Record;
      Successful : Boolean := true;
   begin
      Motors.API.Decrease_Voltage_Decode
        (Message       => Message,
         VoltageOne    => Voltage_One,
         VoltageTwo    => Voltage_Two,
         VoltageThree  => Voltage_Three,
         VoltageFour   => Voltage_Four,
         Time          => Time,
         Decode_Status => Status);
      -- moves the drone
      if (Motor_One - Voltage_One >= 3.0) and (Motor_Two - Voltage_Two >= 3.0) and (Motor_Three - Voltage_Three >= 3.0) and (Motor_Four - Voltage_Four >= 3.0) then
         Motor_One := Motor_One - Voltage_One;
         Motor_Two := Motor_Two - Voltage_Two;
         Motor_Three := Motor_Three - Voltage_Three;
         Motor_Four := Motor_Four - Voltage_Four;
         delay Time;
         Motor_One := Motor_One + Voltage_One;
         Motor_Two := Motor_Two + Voltage_Two;
         Motor_Three := Motor_Three + Voltage_Three;
         Motor_Four := Motor_Four + Voltage_Four;
      else
         Successful := False;
      end if;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID       => 1,
         Successful          => Successful);
      Route_Message (Message => Move_Reply);
   end Handle_Decrease_Voltage_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if Motors.API.Is_Increase_Voltage(Message => Message) then
         Handle_Increase_Voltage_Request(Message);
      elsif Motors.API.Is_Decrease_Voltage(Message => Message) then
         Handle_Decrease_Voltage_Request(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Motors,
                                            CubedOS.Log_Server.API.Error,
                                            "An unknown message type has been received!");
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Motors.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Motors.Messages;
