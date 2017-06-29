<?php
include("config.php");
session_start();

if($_SERVER["REQUEST_METHOD"] == "POST") {
  $verifyUserQuery = "SELECT * FROM Users WHERE user = '{$_POST["username"]}' and password = '{$_POST["password"]}'";
  $result = mysqli_query($connection, $verifyUserQuery);
  $count = mysqli_num_rows($result);

  if ($count == 1) {
    while($row = mysqli_fetch_array($result)) {
      $_SESSION['login_user'] = $row['userID'];
      $_SESSION['role'] = $row['role'];
      header("location: modify.php");
    }
  } else {
    $error = "The username and the password do not match!";
  }
}
?>

<!DOCTYPE html>
<html lang="en">
<head>
  <title>Log In</title>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
</head>
<body>

<div class="container">
  <h2>Log in</h2>
  <form action="" method = "post">
    <div class="form-group">
      <label for="username">Username:</label>
      <input type="username" class="form-control" placeholder="username" name="username">
    </div>
    <div class="form-group">
      <label for="password">Password:</label>
      <input type="password" class="form-control" placeholder="password" name="password">
    </div>
    <? if(isset($error)) { echo $error . "<br>"; } ?>
    <button type="submit" class="btn btn-default">Submit</button>
  </form>

</div>
</body>
</html>
