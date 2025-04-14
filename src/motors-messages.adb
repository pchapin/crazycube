--------------------------------------------------------------------------------
-- FILE   : motors-messages.adb
-- SUBJECT: Body of a package that implements the main part of the motors module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with Name_Resolver;
with Motors.API; use Motors.API; -- Needed so that the types in the API can be used here.
with Sensors.API; use Sensors.API;
with CubedOS.Log_Server.API;
with Ada.Text_IO; use Ada.Text_IO;

package body motors.Messages is
   use Message_Manager;
   Motor_One : Voltage_Type := 30;
   Motor_Two : Voltage_Type := 30;
   Motor_Three : Voltage_Type := 30;
   Motor_Four : Voltage_Type := 30;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Increase_Voltage_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Increase_Voltage(Message)
   is
      Status  : Message_Status_Type;
      Voltage_One : Voltage_Type;
      Voltage_Two : Voltage_Type;
      Voltage_Three : Voltage_Type;
      Voltage_Four: Voltage_Type;
      Time : Time_Type;
      Move_Reply : Message_Record;
      Increase_Sensors : Message_Record;
      Successful : Motors.API.Status_Type := Success;
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
      if (Motor_One + Voltage_One <= 42) and
        (Motor_Two + Voltage_Two <= 42) and
        (Motor_Three + Voltage_Three <= 42) and
        (Motor_Four + Voltage_Four <= 42) then

         Motor_One := Motor_One + Voltage_One;
         Motor_Two := Motor_Two + Voltage_Two;
         Motor_Three := Motor_Three + Voltage_Three;
         Motor_Four := Motor_Four + Voltage_Four;
         delay Duration'Value(Time_Type'Image(Time));
         Motor_One := Motor_One - Voltage_One;
         Motor_Two := Motor_Two - Voltage_Two;
         Motor_Three := Motor_Three - Voltage_Three;
         Motor_Four := Motor_Four - Voltage_Four;

         Increase_Sensors := Sensors.API.Increase_Dumy_Altitude_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1,
            Inches => State_Type'Value(Time_Type'Image(Time)));
         Route_Message(Increase_Sensors);
      else
         Successful := Failure;
      end if;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Name_Resolver.Controller,
         Request_ID       => 1,
         Successful          => Successful);
      Route_Message (Message => Move_Reply);
      Put_Line("reply has been sent. motor_one = " & Voltage_Type'Image(Motor_One));
   end Handle_Increase_Voltage_Request;

   procedure Handle_Decrease_Voltage_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Decrease_Voltage(Message)
   is
      Status  : Message_Status_Type;
      Voltage_One : Voltage_Type;
      Voltage_Two : Voltage_Type;
      Voltage_Three : Voltage_Type;
      Voltage_Four: Voltage_Type;
      Time : Time_Type;
      Move_Reply : Message_Record;
      Decrease_Sensors : Message_Record;
      Successful : Motors.API.Status_Type := Success;
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
      if (Motor_One - Voltage_One >= 30) and
        (Motor_Two - Voltage_Two >= 30) and
        (Motor_Three - Voltage_Three >= 30) and
        (Motor_Four - Voltage_Four >= 30)then

         Put_Line("decresing");
         Motor_One := Motor_One - Voltage_One;
         Motor_Two := Motor_Two - Voltage_Two;
         Motor_Three := Motor_Three - Voltage_Three;
         Motor_Four := Motor_Four - Voltage_Four;

         delay Duration'Value(Time_Type'Image(Time));

         Motor_One := Motor_One + Voltage_One;
         Motor_Two := Motor_Two + Voltage_Two;
         Motor_Three := Motor_Three + Voltage_Three;
         Motor_Four := Motor_Four + Voltage_Four;

         Decrease_Sensors := Sensors.API.Decrease_Dumy_Altitude_Encode
           (Sender_Address => Name_Resolver.Sensors,
            Request_ID => 1,
            Inches => State_Type'Value(Time_Type'Image(Time)));
         Route_Message(Decrease_Sensors);
      else
         Successful := Failure;
      end if;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Name_Resolver.Controller,
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
