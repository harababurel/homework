<!DOCTYPE html>
<html lang="en">
<head>
  <title>Add Cursuri</title>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
</head>
<body>
    <br>
    <h4 align = "center" class ="h4"> Add curs: </h4>
    <form class="form-horizontal" action="/exam/add-curs.php" method="post">
        <div class="form-group">
            <label class="control-label col-sm-2" for="denumire"> denumire: </label>
            <div class="col-sm-10">
                <input type="text" name="denumire" class="form-control" placeholder="denumire" required>
            </div>
        </div>
        <div class="form-group">
            <label class="control-label col-sm-2" for="profesor"> profesor: </label>
            <div class="col-sm-10">
                <input type="text" name="profesor" class="form-control" placeholder="profesor" required>
            </div>
        </div>
        <div class="form-group">
            <label class="control-label col-sm-2" for="durata"> durata: </label>
            <div class="col-sm-10">
                <input type="text" name="durata" class="form-control" placeholder="durata" required>
            </div>
        </div>

        <div class="form-group">
            <div class="col-sm-offset-2 col-sm-10">
                <button type="submit" class="btn btn-default">Submit</button>
            </div>
        </div>
    </form>

    <!--
    <hr>

    <h4 align = "center" class ="h4"> Add news: </h4>
    <form class="form-horizontal" onsubmit="addNews(); return false;">
        <div class="form-group">
            <label class="control-label col-sm-2" for="title"> Title: </label>
            <div class="col-sm-10">
                <input type="text" name="title" class="form-control" placeholder="title" id="textInput" required>
            </div>
        </div>
        <div class="form-group">
            <label class="control-label col-sm-2" for="text"> Description: </label>
            <div class="col-sm-10">
                <input type="text" name="description" class="form-control" placeholder="description" id="descriptionInput" required>
            </div>
        </div>
        <div class="form-group">
            <div class="col-sm-offset-2 col-sm-10">
                <button type="submit" class="btn btn-default">Submit</button>
            </div>
        </div>
    </form>

    <button class="btn btn-default" onclick="addAllNews()">Add all</button>
    -->
    <hr>

</body>
</html>

<script>
cursuri = [];
function addCursuri() {
  cursuri.push([$("#textInput").val(), $("#descriptionInput").val()]);
}

function addAllCursuri() {
  $.post('add-curs.php', {
    data: cursuri
  }, function(data, status) {
    console.log(data, status);
  });
}
</script>
