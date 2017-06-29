<?php
include('config.php');
$data=[];

function getLastFive() {
  $GLOBALS['data'] = mysqli_query($GLOBALS['connection'],"SELECT * FROM Restrictii");
}

function transformIntoHTML() {
  $resultString = "";
  while($row = mysqli_fetch_array($GLOBALS['data'])) {
    $resultString .= '<tr>';
    $resultString .= '<th>' . $row['idcurs'] . '</th>';
    $resultString .= '<th>' . $row['restrictii'] . '</th>';
    $resultString .= '</tr>';
  }
  return $resultString;
}

getLastFive();
echo transformIntoHTML();
?>
