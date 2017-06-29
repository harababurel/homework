<?php
$serverName = "localhost";
$username = "root";
$password = "";
$dbname = "exam";
$tableName = "Sali";

$connection = mysqli_connect($serverName, $username, $password, $dbname);

if (!$connection) {
  die('Could not connect: ' . mysqli_error());
}

?>
