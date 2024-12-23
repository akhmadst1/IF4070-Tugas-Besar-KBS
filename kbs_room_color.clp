; Aturan untuk menampilkan tutorial saat sistem dimuat
(defrule tampilkan-tutorial
   (tutorial)
   =>
   (printout t "=====================================" crlf)
   (printout t "SELAMAT DATANG DI SISTEM REKOMENDASI WARNA RUANGAN!" crlf)
   (printout t "=====================================" crlf)
   (printout t "cara menggunakan aplikasi ini:" crlf)
   (printout t "1. berikan input menggunakan perintah 'assert' dengan template berikut:" crlf)
   (printout t "   (input" crlf)
   (printout t "      (arah timur/barat/utara/selatan)" crlf)
   (printout t "      (kepribadian pemberani/agresif/optimis/ambisius/ceria/atraktif/drama/karismatik/bijaksana/mandiri/individualis/kreatif/intuitif/" crlf)
   (printout t "                   misterius/elegan/jujur/lembut/sabar/sederhana/hangat/ramah/bersemangat/tenang/stabil/rasional/kokoh/" crlf)
   (printout t "                   tradisional/nyaman/unik/inspiratif/kuat/berani/berwibawa/romantis/sensitif/penyayang/bersahabat)" crlf)
   (printout t "      (fungsi-ruangan taman/ruang-olahraga/ruang-tamu/ruang-makan/dapur/kamar-tidur/ruang-keluarga/toilet/ruang-belajar/ruang-kerja)" crlf)
   (printout t "      (ukuran-ruangan kecil/sedang/besar)" crlf)
   (printout t "      (temperatur-dan-iklim panas/tropis/dingin/lembab/sejuk/subtropis)" crlf)
   (printout t "      (bahan semen/batu-bata/bambu/beton/kayu)" crlf)
   (printout t "      (tema energetik/ceria/cerah/elegan/alami/tenang/minimalis/lembut/natural)" crlf)
   (printout t "      (frekuensi-penggunaan sering/jarang))" crlf)
   (printout t "2. jalankan mesin inferensi menggunakan '(run)' untuk melihat rekomendasi." crlf)
   (printout t "3. warna yang direkomendasikan dan persentase kecocokannya akan ditampilkan." crlf)
   (printout t "=====================================" crlf)
   (printout t "contoh input:" crlf)
   (printout t "(assert (input" crlf)
   (printout t "   (arah barat)" crlf)
   (printout t "   (kepribadian pemberani)" crlf)
   (printout t "   (fungsi-ruangan taman)" crlf)
   (printout t "   (ukuran-ruangan besar)))" crlf)
   (printout t "=====================================" crlf)
   (printout t "SELAMAT MENGGUNAKAN APLIKASI!" crlf)
   (printout t "=====================================" crlf))

; Template untuk input - semua slot dibuat OPTIONAL
(deftemplate input
   (multislot arah (default nil))
   (slot kepribadian (default nil))
   (slot fungsi-ruangan (default nil))
   (slot ukuran-ruangan (default nil))
   (slot temperatur-dan-iklim (default nil))
   (slot bahan (default nil))
   (slot tema (default nil))
   (slot frekuensi-penggunaan (default nil)))

; Template untuk hasil
(deftemplate hasil
   (slot klasifikasi)
   (slot score (type INTEGER))
   (slot match-percentage (type FLOAT)))

; Rule untuk klasifikasi warna merah
(defrule klasifikasi-merah
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   ; Evaluasi setiap kriteria
   (if (neq ?arah nil) 
      then 
         (if (eq ?arah barat) then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ pemberani agresif optimis ambisius))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ taman ruang-olahraga))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ sedang besar))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (member$ ?temp (create$ panas tropis))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (member$ ?bahan (create$ semen batu-bata))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema energetik)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi merah)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna jingga
(defrule klasifikasi-jingga
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   ; Evaluasi setiap kriteria
   (if (neq ?arah nil) 
      then 
         (if (eq ?arah barat) then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ ceria atraktif drama karismatik))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ ruang-tamu ruang-olahraga))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (eq ?ukuran besar) then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (member$ ?temp (create$ panas tropis))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (member$ ?bahan (create$ semen bambu))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema ceria)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi jingga)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna kuning
(defrule klasifikasi-kuning
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   ; Evaluasi setiap kriteria
   (if (neq ?arah nil) 
      then 
         (if (eq ?arah barat) then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ bijaksana ceria mandiri individualis))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ ruang-makan dapur))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ kecil sedang))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then
         (if (eq ?temp kuning) then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (member$ ?bahan (create$ semen beton))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema cerah)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi kuning)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna ungu
(defrule klasifikasi-ungu
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (eq ?arah utara) 
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ kreatif intuitif misterius elegan))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ toilet kamar-tidur taman))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (eq ?ukuran sedang)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (member$ ?temp (create$ dingin lembab))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (eq ?bahan semen)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema elegan)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek jarang)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi ungu)
                 (score ?score)
                 (match-percentage ?percentage))))

(defrule klasifikasi-hijau
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin

   (if (neq ?arah nil)
      then 
         (if (eq ?arah selatan) then (bind ?score (+ ?score 1))))

   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ jujur lembut sabar sederhana))
            then (bind ?score (+ ?score 1))))

   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ taman ruang-keluarga))
            then (bind ?score (+ ?score 1))))

   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ sedang besar))
            then (bind ?score (+ ?score 1))))

   (if (neq ?temp nil)
      then 
         (if (eq ?temp sejuk)
            then (bind ?score (+ ?score 1))))

   (if (neq ?bahan nil)
      then 
         (if (member$ ?bahan (create$ semen kayu))
            then (bind ?score (+ ?score 1))))

   (if (neq ?tema nil)
      then 
         (if (eq ?tema alami)
            then (bind ?score (+ ?score 1))))

   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))

   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))

   (assert (hasil (klasifikasi hijau)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna biru
(defrule klasifikasi-biru
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (eq ?arah timur) then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ lembut sabar bijaksana setia))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ toilet kamar-tidur ruang-belajar))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ kecil sedang))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (eq ?temp sejuk)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (eq ?bahan semen)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema tenang)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi biru)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna putih
(defrule klasifikasi-putih
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (eq ?arah timur) 
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ sederhana tenang jujur bijaksana))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ toilet ruang-belajar toilet))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (eq ?ukuran kecil)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (eq ?temp sejuk)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (member$ ?bahan (create$ batu-bata semen))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema minimalis)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi putih)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna peach
(defrule klasifikasi-peach
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (eq ?arah timur) 
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ hangat ramah optimis bersemangat))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ kamar-tidur ruang-keluarga))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ sedang kecil))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (eq ?temp sejuk)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (eq ?bahan semen)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema lembut)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi peach)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna abu-abu
(defrule klasifikasi-abu-abu
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (member$ ?arah (create$ utara selatan)) 
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ tenang bijaksana stabil rasional))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ ruang-kerja ruang-keluarga))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ kecil besar))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (member$ ?temp (create$ dingin subtropis))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (eq ?bahan semen)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema minimalis)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (member$ ?frek (create$ sering jarang))
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi abu-abu)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna coklat
(defrule klasifikasi-coklat
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (eq ?arah selatan) 
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ kokoh hangat tradisional nyaman))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ toilet ruang-makan ruang-tamu))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ sedang besar))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (member$ ?temp (create$ panas tropis))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (eq ?bahan kayu)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema natural)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi coklat)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk klasifikasi warna tosca
(defrule klasifikasi-tosca
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (eq ?arah timur) then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ kreatif ceria unik inspiratif))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (eq ?fungsi taman)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (eq ?ukuran sedang)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
         (if (eq ?temp tropis)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
         (if (eq ?bahan semen)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema ceria)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
         (if (eq ?frek jarang)
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi tosca)
                 (score ?score)
                 (match-percentage ?percentage))))
              
; Rule untuk klasifikasi warna maroon
(defrule klasifikasi-maroon
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin
   
   (if (neq ?arah nil)
      then 
         (if (eq ?arah barat) then (bind ?score (+ ?score 1))))
   
   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ kuat, berani, berwibawa, elegan))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?fungsi nil)
      then 
         (if (eq ?fungsi ruang-makan)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?ukuran nil)
      then 
         (if (eq ?ukuran besar)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?temp nil)
      then 
       (if (member$ ?temp (create$ panas, tropis))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?bahan nil)
      then 
       (if (member$ ?bahan (create$ semen, kayu, batu-bata))
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?tema nil)
      then 
         (if (eq ?tema elegan)
            then (bind ?score (+ ?score 1))))
   
   (if (neq ?frek nil)
      then 
       (if (member$ ?frek (create$ sering, jarang))
            then (bind ?score (+ ?score 1))))
   
   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))
   
   (assert (hasil (klasifikasi maroon)
                 (score ?score)
                 (match-percentage ?percentage))))

(defrule klasifikasi-pink
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin

   (if (neq ?arah nil)
      then 
         (if (eq ?arah timur) then (bind ?score (+ ?score 1))))

   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ romantis lembut sensitif penyayang))
            then (bind ?score (+ ?score 1))))

   (if (neq ?fungsi nil)
      then 
         (if (eq ?fungsi ruang-tidur)
            then (bind ?score (+ ?score 1))))

   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ kecil sedang))
            then (bind ?score (+ ?score 1))))

   (if (neq ?temp nil)
      then 
       (if (member$ ?temp (create$ sejuk tropis))
            then (bind ?score (+ ?score 1))))

   (if (neq ?bahan nil)
      then 
       (if (eq ?bahan semen)
            then (bind ?score (+ ?score 1))))

   (if (neq ?tema nil)
      then 
         (if (eq ?tema elegan)
            then (bind ?score (+ ?score 1))))

   (if (neq ?frek nil)
      then 
       (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))

   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))

   (assert (hasil (klasifikasi pink)
                 (score ?score)
                 (match-percentage ?percentage))))

(defrule klasifikasi-navy
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin

   (if (neq ?arah nil)
      then 
         (if (eq ?arah selatan) then (bind ?score (+ ?score 1))))

   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ tenang elegan misterius stabil))
            then (bind ?score (+ ?score 1))))

   (if (neq ?fungsi nil)
      then 
         (if (eq ?fungsi ruang-kerja)
            then (bind ?score (+ ?score 1))))

   (if (neq ?ukuran nil)
      then 
         (if (eq ?ukuran besar)
            then (bind ?score (+ ?score 1))))

   (if (neq ?temp nil)
      then 
       (if (member$ ?temp (create$ dingin subtropis))
            then (bind ?score (+ ?score 1))))

   (if (neq ?bahan nil)
      then 
       (if (eq ?bahan semen)
            then (bind ?score (+ ?score 1))))

   (if (neq ?tema nil)
      then 
         (if (eq ?tema tenang)
            then (bind ?score (+ ?score 1))))

   (if (neq ?frek nil)
      then 
       (if (eq ?frek jarang)
            then (bind ?score (+ ?score 1))))

   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))

   (assert (hasil (klasifikasi navy)
                 (score ?score)
                 (match-percentage ?percentage))))

(defrule klasifikasi-krem
   (input (arah ?arah)
          (kepribadian ?kepribadian)
          (fungsi-ruangan ?fungsi)
          (ukuran-ruangan ?ukuran)
          (temperatur-dan-iklim ?temp)
          (bahan ?bahan)
          (tema ?tema)
          (frekuensi-penggunaan ?frek))
   =>
   (bind ?score 0)
   (bind ?total-specs 8)  ; Total spesifikasi yang mungkin

   (if (neq ?arah nil)
      then 
         (if (eq ?arah barat) then (bind ?score (+ ?score 1))))

   (if (neq ?kepribadian nil)
      then 
         (if (member$ ?kepribadian (create$ sederhana tenang bersahabat nyaman))
            then (bind ?score (+ ?score 1))))

   (if (neq ?fungsi nil)
      then 
         (if (member$ ?fungsi (create$ ruang-keluarga ruang-tidur))
            then (bind ?score (+ ?score 1))))

   (if (neq ?ukuran nil)
      then 
         (if (member$ ?ukuran (create$ sedang besar))
            then (bind ?score (+ ?score 1))))

   (if (neq ?temp nil)
      then 
       (if (eq ?temp tropis)
            then (bind ?score (+ ?score 1))))

   (if (neq ?bahan nil)
      then 
       (if (member$ ?bahan (create$ semen kayu))
            then (bind ?score (+ ?score 1))))

   (if (neq ?tema nil)
      then 
         (if (eq ?tema elegan)
            then (bind ?score (+ ?score 1))))

   (if (neq ?frek nil)
      then 
       (if (eq ?frek sering)
            then (bind ?score (+ ?score 1))))

   ; Hitung persentase dari total semua spesifikasi
   (bind ?percentage (* (/ ?score ?total-specs) 100))

   (assert (hasil (klasifikasi krem)
                 (score ?score)
                 (match-percentage ?percentage))))

; Rule untuk menampilkan hasil dengan skor tertinggi
(defrule tampilkan-skor-tertinggi
   (declare (salience -10))
   ?h1 <- (hasil (klasifikasi ?k1) (score ?s1) (match-percentage ?p1))
   (not (hasil (klasifikasi ?k2) (score ?s2&:(> ?s2 ?s1))))
   =>
   (printout t "=====================================" crlf)
   (printout t "HASIL REKOMENDASI WARNA" crlf)
   (printout t "=====================================" crlf)
   (printout t "Warna yang Direkomendasikan: " ?k1 crlf)
   (printout t "Skor: " ?s1 crlf)
   (printout t "Persentase Kecocokan: " ?p1 "%" crlf)
   (printout t "=====================================" crlf))