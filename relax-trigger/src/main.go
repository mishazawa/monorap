package main

import (
	"fmt"
	"flag"
	"time"
	"github.com/gen2brain/beeep"
	)


func main() {
	work_time := flag.Int("work", 30, "Work time")
	relax_time := flag.Int("relax", 30, "Relax time")

	flag.Parse()

	for {
		Work(*work_time);
		for i := 0; i < *work_time; i++ {
			fmt.Printf("continue work %v minutes.\n", *work_time - i)
			time.Sleep(time.Minute)
		}
		Relax(*relax_time);
		for i := 0; i < *relax_time; i++ {
			fmt.Printf("continue relax %v minutes.\n", *relax_time - i)
			time.Sleep(time.Minute)
		}
	}
}


func Work(t int) {
	message("Work", t, "assets/work.png")
}

func Relax(t int) {
	message("Relax", t, "assets/relax.png")
}


func message (mess string, t int, img string) {
	err := beeep.Notify("Relax trigger", fmt.Sprintf("%s for %d minutes", mess, t), img)
	if err != nil {
		panic(err)
	}
}
