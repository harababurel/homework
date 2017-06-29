
<?php
include ("session.php");
?>


<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.0/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
</head>
<body>

<div class="container">

<br>

  <a href="add-curs.php" class="btn btn-default btn-sm">
    <i class="fa fa-plus" aria-hidden="true"></i> Add curs
  </a>

  <a href="add-restrictii.php" class="btn btn-default btn-sm">
    <i class="fa fa-plus" aria-hidden="true"></i> Add restrictii
  </a>

  <a href="add-planificare.php" class="btn btn-default btn-sm">
    <i class="fa fa-plus" aria-hidden="true"></i> Add planificare
  </a>



    <h1 align = "center" class="page-header">Cursuri</h1>
    <p id="filterP"> </p>
    <table class="table table-bordered">
        <tr>
            <th>ID</th>
            <th>Denumire</th>
            <th>Profesor</th>
            <th>Durata</th>
        </tr>
        <tbody id="dataDivCursuri"> </tbody>
    </table>

    <h1 align = "center" class="page-header">Sali</h1>
    <table class="table table-bordered">
        <tr>
            <th>denumire</th>
            <th>capacitate</th>
        </tr>
        <tbody id="dataDivSali"> </tbody>
    </table>

    <h1 align = "center" class="page-header">Restrictii</h1>
    <table class="table table-bordered">
        <tr>
            <th>idcurs</th>
            <th>restrictii</th>
        </tr>
        <tbody id="dataDivRestrictii"> </tbody>
    </table>





    <h1 align = "center" class="page-header">Orar</h1>
    <table class="table table-bordered">
      <thead>
        <tr>
          <th>luni</th>
          <th>marti</th>
          <th>miercuri</th>
          <th>joi</th>
          <th>vineri</th>
          <th>sambata</th>
          <th>duminica</th>
        </tr>
      </thead>
      <tbody>

<?php
include('config.php');
$zile = array("luni", "marti", "miercuri", "joi", "vineri", "sambata", "duminica");
for($ora=8; $ora<=20; $ora+=2) {
  echo "<tr>";

  foreach($zile as $zi) {
    $planificari = mysqli_query($GLOBALS['connection'],"SELECT idcurs, idsala FROM Orar WHERE orainceput=".$ora." AND zi='".$zi."' LIMIT 1;");
    $row = mysqli_fetch_assoc($planificari);

    echo "<td>";

    if($row) {

      $cursuri = mysqli_query($GLOBALS['connection'],"SELECT denumire, profesor, durata FROM Cursuri WHERE id=".$row['idcurs'].";");
      $curs = mysqli_fetch_assoc($cursuri);

      $sali = mysqli_query($GLOBALS['connection'],"SELECT denumire FROM Sali WHERE id=".$row['idsala'].";");
      $sala = mysqli_fetch_assoc($sali);



      if($curs) {
        $orasfarsit = $ora + $curs['durata'];

        echo "<span class='label label-info'>".$ora."&rarr;".$orasfarsit."</span> ".$curs['denumire'] . " cu " . $curs['profesor'];
        if($sala) {
          echo " in <span class='label label-warning'>" . $sala['denumire'] . "</span>";
        }
      }
    }

    echo "</td>";
  }

  echo "</tr>";
}

?>

      </tbody>
    </table>


</body>
<script>
$(document).ready(function() {
  $.get('view-cursuri.php', {}, function(data, status) {
    console.log(data);
    document.getElementById("dataDivCursuri").innerHTML = data;
  });
});

$(document).ready(function() {
  $.get('view-sali.php', {}, function(data, status) {
    console.log(data);
    document.getElementById("dataDivSali").innerHTML = data;
  });
});


$(document).ready(function() {
  $.get('view-restrictii.php', {}, function(data, status) {
    console.log(data);
    document.getElementById("dataDivRestrictii").innerHTML = data;
  });
});

</script>
</div>
</html>
