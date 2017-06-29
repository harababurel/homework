<!DOCTYPE html>
<html lang="en">
<head>
  <title>Planificare Cursuri</title>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
</head>
<body>
    <br>

<div class="container">
    <h4 align = "center" class ="h4">Add Planificare: </h4>
    <br><br>

    <form class="form-horizontal col-sm-offset-5 col-sm-2" action="/exam/add-planificare.php" method="post">

      <div class="form-group">
          <label for="cursselect">Curs</label>
          <select class="form-control" name="cursselect">
<?php
include('config.php');

$cursuri = mysqli_query($GLOBALS['connection'],"SELECT * FROM Cursuri");

while($row = mysqli_fetch_array($cursuri)) {
  echo "<option value=" . $row['id'] . ">" . $row['denumire'] . "</option>";
}
?>
          </select>

      </div>

      <div class="form-group">
          <label for="salaselect">Sala</label>
          <select class="form-control" name="salaselect">
<?php
include('config.php');

$sali = mysqli_query($GLOBALS['connection'],"SELECT * FROM Sali");

while($row = mysqli_fetch_array($sali)) {
  echo "<option value=" . $row['id'] . ">" . $row['denumire'] . " (" . $row['capacitate'] . " locuri)</option>";
}
?>
          </select>

      </div>

      <div class="form-group">
          <label for="ziselect">Zi</label>
          <select class="form-control" name="ziselect">
            <option value="luni">luni</option>
            <option value="marti">marti</option>
            <option value="miercuri">miercuri</option>
            <option value="joi">joi</option>
            <option value="vineri">vineri</option>
            <option value="sambata">sambata</option>
            <option value="duminica">duminica</option>
          </select>
      </div>

      <div class="form-group">
          <label for="oraselect">Ora Inceput</label>
          <select class="form-control" name="oraselect">

            <?php
            for($x=8; $x<=20; $x += 2) {
              echo "<option value=" . $x . ">" . $x . "</option>";
            }
            ?>
          </select>
      </div>


        <div class="form-group">
            <div class="col-sm-offset-2 col-sm-10">
                <button type="submit" class="btn btn-default">Submit</button>
            </div>
        </div>
    </form>

</div>
</body>
</html>

<script>
planificari = [];
function addPlanificari() {
  cursuri.push([$("#textInput").val(), $("#descriptionInput").val()]);
}

function addAllPlanificari() {
  $.post('add-planificare.php', {
  data: planificari
}, function(data, status) {
  console.log(data, status);
});
}
</script>
