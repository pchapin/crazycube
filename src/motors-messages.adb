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
   Motor_One : Voltage_Type := 20; -- with a voltage of 20, the motors are not moving
   Motor_Two : Voltage_Type := 20;
   Motor_Three : Voltage_Type := 20;
   Motor_Four : Voltage_Type := 20;

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
        (Motor_Four + Voltage_Four <= 42) and
        Status = Success then

         Motor_One := Motor_One + Voltage_One;
         Motor_Two := Motor_Two + Voltage_Two;
         Motor_Three := Motor_Three + Voltage_Three;
         Motor_Four := Motor_Four + Voltage_Four;
         delay Duration'Value(Time_Type'Image(Time))/2.0;
         Motor_One := Motor_One - Voltage_One;
         Motor_Two := Motor_Two - Voltage_Two;
         Motor_Three := Motor_Three - Voltage_Three;
         Motor_Four := Motor_Four - Voltage_Four;

         if Voltage_One > 0 and Voltage_Two > 0 and Voltage_Three > 0 and Voltage_Four > 0 then
            Increase_Sensors := Sensors.API.Increase_Dumy_Altitude_Request_Encode
              (Sender_Address => Name_Resolver.Sensors,
               Request_ID => 1,
               Inches => State_Type'Value(Time_Type'Image(Time)));
            Route_Message(Increase_Sensors);
         end if;

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
        (Motor_Four - Voltage_Four >= 30) and
        Status = Success then

         Put_Line("decresing");
         Motor_One := Motor_One - Voltage_One;
         Motor_Two := Motor_Two - Voltage_Two;
         Motor_Three := Motor_Three - Voltage_Three;
         Motor_Four := Motor_Four - Voltage_Four;

         delay Duration'Value(Time_Type'Image(Time))/2.0;

         Motor_One := Motor_One + Voltage_One;
         Motor_Two := Motor_Two + Voltage_Two;
         Motor_Three := Motor_Three + Voltage_Three;
         Motor_Four := Motor_Four + Voltage_Four;

         if Voltage_One > 0 and Voltage_Two > 0 and Voltage_Three > 0 and Voltage_Four > 0 then
            Decrease_Sensors := Sensors.API.Decrease_Dumy_Altitude_Request_Encode
              (Sender_Address => Name_Resolver.Sensors,
               Request_ID => 1,
               Inches => State_Type'Value(Time_Type'Image(Time)));
            Route_Message(Decrease_Sensors);
         end if;
      else
         Successful := Failure;
      end if;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Name_Resolver.Controller,
         Request_ID       => 1,
         Successful          => Successful);
      Route_Message (Message => Move_Reply);
   end Handle_Decrease_Voltage_Request;

   procedure Handle_Launch_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Launch_Request(Message)
   is

   begin
      Motor_One := 35;
      Motor_Two := 35;
      Motor_Three := 35;
      Motor_Four := 35;

      delay 1.5;

      Motor_One := 30;
      Motor_Two := 30;
      Motor_Three := 30;
      Motor_Four := 30;

      Route_Message(Sensors.API.Increase_Dumy_Altitude_Request_Encode
                      (Sender_Address => Name_Resolver.Sensors,
                       Request_ID     => 1,
                       Inches         => 3));

      Route_Message(Motors.API.Launch_Reply_Encode
                      (Receiver_Address => Name_Resolver.Controller,
                       Request_ID       => 1,
                       Successful       => Success));
   end Handle_Launch_Request;

   procedure Handle_Land_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Land_Request(Message)
   is
      Time : Time_Type;
      Status : Message_Status_Type;
      Successful : Motors.API.Status_Type := Success;
   begin
      Motors.API.Land_Request_Decode
        (Message       => Message,
         Time          => Time,
         Decode_Status => Status);
      if Status = Success then
         Motor_One := 25;
         Motor_Two := 25;
         Motor_Three := 25;
         Motor_Four := 25;

         delay Duration'Value(Time_Type'Image(Time))/2.0;

         Motor_One := 20;
         Motor_Two := 20;
         Motor_Three := 20;
         Motor_Four := 20;

         Route_Message(Sensors.API.Decrease_Dumy_Altitude_Request_Encode
                      (Sender_Address => Name_Resolver.Sensors,
                       Request_ID     => 1,
                       Inches         => State_Type'Value(Time_Type'Image(Time))));
      else
         Successful := Failure;
      end if;
      Route_Message(Motors.API.Land_Reply_Encode
                      (Receiver_Address => Name_Resolver.Controller,
                       Request_ID       => 1,
                       Successful        => Successful));
   end Handle_Land_Request;
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
      elsif Sensors.API.Is_Decrease_Dumy_Altitude_Reply(Message => Message) then
         Put_Line("Altitude has decreased");
      elsif Sensors.API.Is_Increase_Dumy_Altitude_Reply(Message => Message) then
         Put_Line("Altitude has Increased");
      elsif Motors.API.Is_Launch_Request(Message => Message) then
         Handle_Launch_Request(Message);
      elsif Motors.API.Is_Land_Request(Message => Message) then
         Handle_Land_Request(Message);
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
