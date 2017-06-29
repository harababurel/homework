<?php
include ("session.php");

if (!isset($_POST['title']))  {
  header("Location: http://eagle/exam24/log-in.php");
}

$deleteQuery = "DELETE FROM News WHERE title = " .
  "'{$_POST["title"]}' " .
  " AND userID = " .
  "{$_SESSION['login_user']};";

$connection->query($deleteQuery);
mysqli_close($connection);
header("Location: http://eagle/exam24/modify.php");
?>
