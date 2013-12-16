       identification division.
       program-id. weather.

       environment division.

       input-output section.

       file-control.

           select tempfile-for-location
               assign to tempfile-for-location-name
               file status is tempfile-for-location-status
               organization is line sequential.


           select tempfile-for-weather
               assign to tempfile-for-weather-name
               file status is tempfile-for-weather-status
               organization is line sequential.

       data division.

       file section.

       fd  tempfile-for-location.
       01  tempfile-for-location-record   pic x(255).

       fd  tempfile-for-weather.
       01  tempfile-for-weather-record    pic x(255).

       working-storage section.

       01  weather-api-key                pic x(255) 
                                          value "pksfrqzus63xbb34yt4dt5vq".

       01  location                       pic x(255).
       01  request-url                    pic x(1024).

       01  return-value                   pic 9(8) comp value zeroes.

       01  tempfile-for-location-name     pic x(64) value './data.tmp'.
       01  tempfile-for-location-status   pic x(2).

       01  tempfile-for-weather-name      pic x(64) value './data2.tmp'.
       01  tempfile-for-weather-status    pic x(2).

       01  weather-result pic x(1024).

       01  location-columns.

           03  replystatus     pic x(100).
           03  country-name    pic x(100).
           03  country-code    pic x(100).
           03  region-code     pic x(100).
           03  region-name     pic x(100).
           03  city-name       pic x(100).
           03  zipcode         pic x(100).
           03  latitude        pic x(100).
           03  longitude       pic x(100).
           03  timezone        pic x(100).
           03  org-name        pic x(100).
           03  isp-name        pic x(100).
           03  as-number-name  pic x(100).
           03  dns-server      pic x(100).


       01  weather-columns.

           03  hour            pic x(100).
           03  celsius         pic x(100).
           03  weather-code    pic x(100).
           03  weather-icon    pic x(100).
           03  weather-desc    pic x(100).
           03  windspeed-m     pic x(100).
           03  windspeed-k     pic x(100).
           03  winddir-d       pic x(100).
           03  winddir-p       pic x(100).
           03  precip          pic x(100).
           03  humidity        pic x(100).
           03  visibility      pic x(100).
           03  pressure        pic x(100).
           03  cloudcover      pic x(100).



        procedure division.


        get-location.

           call "system" using "curl -s http://ip-api.com/csv>data.tmp".

           open input tempfile-for-location

           call 'checkfilestatus' using tempfile-for-location-name tempfile-for-location-status

           read tempfile-for-location

           unstring tempfile-for-location-record delimited by ',' into

                    replystatus
                    country-name
                    country-code
                    region-code
                    region-name
                    city-name
                    zipcode
                    latitude
                    longitude
                    timezone
                    org-name
                    isp-name
                    as-number-name
                    dns-server

            move 
                function concatenate(
                            function trim(city-name);
                            ",";
                            function trim(country-name)) 
                to location.

            display "Location: " function trim(location). 

            close tempfile-for-location.

            call "system" using "rm data.tmp".


        get-weather.

            move
                function concatenate(
                            "curl -s 'http://api.worldweatheronline.com/free/v1/weather.ashx?q=";
                            function trim(location);
                            "&format=csv&num_of_days=0&show_comments=no&key=";
                            function trim(weather-api-key);
                            "' >data2.tmp")
                to request-url.

            call "system" using function trim(request-url).

            open input tempfile-for-weather
           
            call 'checkfilestatus' using tempfile-for-weather-name tempfile-for-weather-status

            read tempfile-for-weather

            unstring tempfile-for-weather-record delimited by ',' into

                     hour   
                     celsius  
                     weather-code 
                     weather-icon  
                     weather-desc 
                     windspeed-m  
                     windspeed-k 
                     winddir-d  
                     winddir-p 
                     precip  
                     humidity  
                     visibility   
                     pressure 
                     cloudcover    

            move
                function concatenate(
                            function trim(celsius);
                            "°C, ";
                            function trim(weather-desc);
                            ". Humidity: ";
                            function trim(humidity);
                            "%, Pressure: ";
                            function trim(pressure);
                            "hPA.") 
                to weather-result;

            close tempfile-for-weather.

            call "system" using "rm data2.tmp".


        display function trim(weather-result).


        *> helper function for file status from: 
        *> http://sourceforge.net/p/open-cobol/discussion/2526793/thread/1183a23c/

        identification division.
        program-id. checkfilestatus.

        data division.

        working-storage section.

        01  status-message pic x(72).
        01  display-message pic x(72) value spaces.

        linkage section.

        01  file-name pic x(64).
        01  file-status pic x(2).

        procedure division using file-name file-status.

        start-checkfilestatus.

           if file-status = '00'
               goback
           end-if

           evaluate file-status

               when 00 move 'SUCCESS.' TO status-message   
               when 02 move 'SUCCESS DUPLICATE.' TO status-message 
               when 04 move 'SUCCESS INCOMPLETE.' TO status-message 
               when 05 move 'SUCCESS OPTIONAL.' TO status-message 
               when 07 move 'SUCCESS NO UNIT.' TO status-message 
               when 10 move 'END OF FILE.' TO status-message 
               when 14 move 'OUT OF KEY RANGE.' TO status-message 
               when 21 move 'KEY INVALID.' TO status-message 
               when 22 move 'KEY EXISTS.' TO status-message 
               when 23 move 'KEY NOT EXISTS.' TO status-message 
               when 30 move 'PERMANENT ERROR.' TO status-message 
               when 31 move 'INCONSISTENT FILENAME.' TO status-message 
               when 34 move 'BOUNDARY VIOLATION.' TO status-message 
               when 35 move 'FILE NOT FOUND.' TO status-message 
               when 37 move 'PERMISSION DENIED.' TO status-message 
               when 38 move 'CLOSED WITH LOCK.' TO status-message 
               when 39 move 'CONFLICT ATTRIBUTE.' TO status-message 
               when 41 move 'ALREADY OPEN.' TO status-message 
               when 42 move 'NOT OPEN.' TO status-message 
               when 43 move 'READ NOT DONE.' TO status-message 
               when 44 move 'RECORD OVERFLOW.' TO status-message 
               when 46 move 'READ ERROR.' TO status-message 
               when 47 move 'INPUT DENIED.' TO status-message 
               when 48 move 'OUTPUT DENIED.' TO status-message 
               when 49 move 'I/O DENIED.' TO status-message 
               when 51 move 'RECORD LOCKED.' TO status-message 
               when 52 move 'END-OF-PAGE.' TO status-message 
               when 57 move 'I/O LINAGE.' TO status-message 
               when 61 move 'FILE SHARING FAILURE.' TO status-message 
               when 91 move 'FILE NOT AVAILABLE.' TO status-message 

           end-evaluate

           string 'ERROR ' delimited by size
               file-name delimited by space
               space delimited by size
               status-message delimited by '.'
               into display-message
           display display-message
           stop run
           .

      end program checkfilestatus.

      end program weather.