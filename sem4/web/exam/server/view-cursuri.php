<?php
include('config.php');
$data=[];

function getLastFive() {
  $GLOBALS['data'] = mysqli_query($GLOBALS['connection'],"SELECT * FROM Cursuri");
}

function transformIntoHTML() {
  $resultString = "";
  while($row = mysqli_fetch_array($GLOBALS['data'])) {
    $resultString .= '<tr>';
    $resultString .= '<th>' . $row['id'] . '</th>';
    $resultString .= '<th>' . $row['denumire'] . '</th>';
    $resultString .= '<th>' . $row['profesor'] . '</th>';
    $resultString .= '<th>' . $row['durata'] . '</th>';
    $resultString .= '</tr>';
  }
  return $resultString;
}

getLastFive();
echo transformIntoHTML();
?>
