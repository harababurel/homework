<?php
include ("session.php");

$currentDate = date("Y-m-d H:i:s");

if ($_SERVER['REQUEST_METHOD'] === 'POST') {
  // The request is using the POST method

  $cursuri = mysqli_query($GLOBALS['connection'],"SELECT * FROM Cursuri WHERE id=" . $_POST["cursselect"] . ";");
  while($row = mysqli_fetch_array($cursuri)) {
    $durata = $row['durata'];
  }

  $orainceput = (int)$_POST["oraselect"];
  $orasfarsit = $orainceput + $durata;

  $insertQuery = "INSERT INTO Orar (idcurs, idsala, zi, orainceput, orasfarsit) VALUES ( ".
    "'{$_POST["cursselect"]}', " .
    "'{$_POST["salaselect"]}', " .
    "'{$_POST["ziselect"]}', " .
    "{$orainceput}, " .
    "{$orasfarsit});";

  $connection->query($insertQuery);
  mysqli_close($connection);

  echo $insertQuery;
  header("Location: http://localhost/exam");
} else {
  header("Location: http://localhost/exam/modify-planificare.php");
}
?>
