package ml.sergiu.mobile_exam;

import android.arch.persistence.room.Dao;
import android.arch.persistence.room.Delete;
import android.arch.persistence.room.Insert;
import android.arch.persistence.room.Query;

import java.util.List;

@Dao
public interface SeatDao {

    @Query("SELECT * FROM seat")
    List<Seat> getAll();

    @Query("SELECT * FROM seat where name LIKE :name")
    Seat findByName(String name);

    @Query("SELECT COUNT(*) from seat")
    int countSeats();

    @Insert
    void insertAll(List<Seat> seats);

    @Delete
    void delete(Seat seat);

    @Query("DELETE FROM seat")
    void deleteAll();

//    @Query("UPDATE seat SET status='reserved' where name LIKE :name")
//    void reserve(Seat seat);
}
