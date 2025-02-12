--------------------------------------------------------------------------------
-- FILE   : sensor-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;    -- See the comments in sensor-api.ads.
with Name_Resolver;      -- See the comments in sensor-api.ads.
with Sensor.API;         -- Needed so that the types in the API can be used here.
with CubedOS.Log_Server.API;

package body Sensor.Messages is
   use Message_Manager;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Get_Gyro_Measurements_Request(Message : in Message_Record)
     with Pre => Sensor.API.Is_Get_Gyro_Measurements_Request(Message)
   is
      Measurments_Reply : Message_Record;
   begin
      Measurments_Reply := Sensors.API.Get_Gyro_Measurements_Reply_Encode
        (
         Measurments_Reply,
         1,
         True,
         0,
         0,
         0);
      Route_Message(Measurments_Reply);
   end Handle_Get_Gyro_Measurements_Request;

   procedure Handle_Get_Accel_Measurements_Request(Message : in Message_Record)
     with Pre => Sensor.API.Is_Get_Accel_Measurements_Request(Message)
   is
      Measurments_Reply : Message_Record;
   begin
      Measurments_Reply := Sensors.API.Get_Accel_Measurements_Reply_Encode
        (
         Measurments_Reply,
         1,
         True,
         0,
         0,
         0);
      Route_Message(Measurments_Reply);
   end Handle_Get_Accel_Measurements_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   procedure Process(Message : in Message_Record) is
   begin
      if Sensor.API.Is_Get_Gyro_Measurements_Request(Message) then
         Handle_Get_Gyro_Measurements_Request(Message);
      elsif Sensor.API.Is_Get_Accel_Measurements_Request(Message) then
         Handle_Get_Accel_Measurements_Request(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.sensor,
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
         Message_Manager.Fetch_Message(Name_Resolver.Sensor.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Sensor.Messages;
