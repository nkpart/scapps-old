jars = Dir['lib/*.jar']

puts "scala -cp #{jars.join(':')}:build/scapps.jar com.scapps.main.Scapps"
