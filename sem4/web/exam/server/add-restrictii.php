<?php
include ("session.php");

$currentDate = date("Y-m-d H:i:s");

if ($_SERVER['REQUEST_METHOD'] === 'POST') {
  // The request is using the POST method
  $insertQuery = "INSERT INTO Restrictii (idcurs, restrictii) VALUES ( ".
    "'{$_POST["idcurs"]}', " .
    "'{$_POST["restrictii"]}');";

  $connection->query($insertQuery);
  mysqli_close($connection);

  echo $insertQuery;
  header("Location: http://localhost/exam");
} else {
  header("Location: http://localhost/exam/modify-restrictii.php");
}
?>
