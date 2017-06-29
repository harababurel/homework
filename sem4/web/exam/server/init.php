<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

$serverName = "localhost";
$username = "root";
$password = "";
$dbname = "exam";
$tableName = "Sali";

$connection = mysqli_connect($serverName, $username, $password);

if (!$connection) {
  die('Could not connect: ' . mysqli_error());
}

if (!$connection->select_db($dbname)) {
  $createDatabaseQuery = "CREATE DATABASE " . $dbname;
  $connection->query($createDatabaseQuery);
  $connection->select_db($dbname);
}

if (!mysqli_query($connection, 'select 1 from ' . $tableName . ' LIMIT 1;')) {
  $createTableQuery = "CREATE TABLE " . $tableName . " ( " .
    "id INT NOT NULL AUTO_INCREMENT, " .
    "denumire VARCHAR(255), "  .
    "capacitate INT, " .
    "PRIMARY KEY ( id ) );";
  $connection->query($createTableQuery);
  $insertQuery = 'INSERT INTO Sali (denumire, capacitate) VALUES ("L338", 20);';
  echo $insertQuery;
  $connection->query($insertQuery);

}

if (!mysqli_query($connection, 'select 1 from Cursuri LIMIT 1;')) {
  $createTableQuery = "CREATE TABLE Cursuri ( " .
    "id INT NOT NULL AUTO_INCREMENT, " .
    "denumire VARCHAR(255) NOT NULL, " .
    "profesor VARCHAR(255) NOT NULL, " .
    "durata INT, " .
    "PRIMARY KEY (id));";
  $connection->query($createTableQuery);
  $insertQuery = 'INSERT INTO Cursuri (denumire, profesor, durata) VALUES ("Programare Web", "Forest", 2);';
  echo $insertQuery;
  $connection->query($insertQuery);
}

if (!mysqli_query($connection, 'select 1 from Restrictii LIMIT 1;')) {
  $createTableQuery = "CREATE TABLE Restrictii ( " .
    "idcurs INT NOT NULL, " .
    "restrictii VARCHAR(255) NOT NULL, " .
    "PRIMARY KEY (idcurs));";
  $connection->query($createTableQuery);
  $insertQuery = 'INSERT INTO Restrictii (idcurs, restrictii) VALUES (1, "[1]");';
  echo $insertQuery;
  $connection->query($insertQuery);
}

if (!mysqli_query($connection, 'select 1 from Orar LIMIT 1;')) {
  $createTableQuery = "CREATE TABLE Orar ( " .
    "id INT NOT NULL AUTO_INCREMENT, " .
    "idcurs INT, " .
    "idsala INT, " .
    "zi VARCHAR(255), " .
    "orainceput INT, " .
    "orasfarsit INT, " .
    "PRIMARY KEY (id));";
  $connection->query($createTableQuery);
  /* $insertQuery = 'INSERT INTO Sali (denumire, capacitate) VALUES ("L338", 20);'; */
  /* echo $insertQuery; */
  /* $connection->query($insertQuery); */

  /* $insertAdminQuery = 'INSERT INTO Users (user, password, role) VALUES ("admin", "pass", 1);'; */
  /* $connection->query($insertAdminQuery); */
}



mysqli_close($connection);
?>

