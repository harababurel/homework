<?php
include('config.php');
$data=[];

function getLastFive() {
  $GLOBALS['data'] = mysqli_query($GLOBALS['connection'],"SELECT * FROM Sali");
}

function transformIntoHTML() {
  $resultString = "";
  while($row = mysqli_fetch_array($GLOBALS['data'])) {
    $resultString .= '<tr>';
    $resultString .= '<th>' . $row['denumire'] . '</th>';
    $resultString .= '<th>' . $row['capacitate'] . '</th>';
    $resultString .= '</tr>';
  }
  return $resultString;
}

getLastFive();
echo transformIntoHTML();
?>
