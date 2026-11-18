# =============================================================================
# share_utils.R  共有用PDF（_share.pdf）を作るための共通関数
#
# 使い方：Rmd の最初のチャンクで
#   source("share_utils.R")      # Rmd と同じフォルダに置く
#   SHARE    <- isTRUE(params$share_mode)
#   MIN_CELL <- params$min_cell
# とする。各関数は share = SHARE, min_cell = MIN_CELL を既定で受け取る。
#
# share_mode = TRUE のときの方針（プロジェクトのルール）
#   ・件数が min_cell 未満のセルは「—」にする（引き算で戻せないよう、行内で1つだけ
#     伏せた場合は、次に小さいセルも伏せる）
#   ・平均・割合も、その計算に使った件数が min_cell 未満なら「—」にする
#   ・最大値・最小値、個別の値、IDの一覧、範囲チェック、パネル整合性の表は出さない
#     （該当チャンクは eval = !SHARE にする。下の sh_private() も使える）
# =============================================================================

if (!exists("SHARE"))    SHARE    <- FALSE
if (!exists("MIN_CELL")) MIN_CELL <- 10

# 出力先の安全確認：Claude が閲覧できるフォルダ（Claude outputs）や Dropbox に
# データ・PDF を書き出さないようにする
sh_check_dir <- function(path) {
  p <- normalizePath(path, mustWork = FALSE)
  if (grepl("Claude outputs|Dropbox", p)) {
    stop("出力先が Claude outputs / Dropbox の中になっています: ", p,
         "\n データや Knit 出力は works フォルダの中に出力してください。")
  }
  invisible(p)
}

# 件数ベクトルを伏せる（share でなければそのまま返す）
sh_n <- function(n, share = SHARE, min_cell = MIN_CELL) {
  if (!share) return(format(n, big.mark = ","))
  ifelse(is.na(n), "—", ifelse(n < min_cell, "—", format(n, big.mark = ",")))
}

# 部分の件数 n を、全体 total と並べて表示するとき用。
# n が小さい場合だけでなく、残り（total − n）が小さい場合も、引き算で少数のセルが
# 戻せてしまうので伏せる（例：100世帯中97世帯 → 残り3世帯が分かる）
sh_n_compl <- function(n, total, share = SHARE, min_cell = MIN_CELL) {
  out <- sh_n(n, share = FALSE)
  if (share) {
    r <- total - n
    out[is.na(n) | n < min_cell | (r > 0 & r < min_cell)] <- "—"
  }
  out
}

# 平均・割合を、計算に使った件数 n とあわせて受け取り、n が小さければ伏せる
sh_stat <- function(x, n, digits = 3, share = SHARE, min_cell = MIN_CELL) {
  out <- ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
  if (share) out[!is.na(n) & n < min_cell] <- "—"
  out
}

# 件数の表（data.frame）の count_cols 列を伏せる。
# 行の中で伏せたセルが1つだけなら、合計からの引き算で戻せるので、次に小さいセルも伏せる
# （列方向にも同じ処理をする）。
sh_mask_counts <- function(df, count_cols, share = SHARE, min_cell = MIN_CELL,
                           secondary = TRUE) {
  if (!share) return(df)
  m <- as.matrix(df[count_cols])
  storage.mode(m) <- "double"
  hide <- !is.na(m) & m < min_cell & m > 0
  if (secondary && ncol(m) >= 2) {
    fix_one <- function(h, v) {
      if (sum(h) == 1 && sum(!h & !is.na(v)) >= 1) {
        cand <- which(!h & !is.na(v))
        h[cand[which.min(v[cand])]] <- TRUE
      }
      h
    }
    for (i in seq_len(nrow(m))) hide[i, ] <- fix_one(hide[i, ], m[i, ])
    if (nrow(m) >= 2) for (j in seq_len(ncol(m))) hide[, j] <- fix_one(hide[, j], m[, j])
  }
  out <- df
  for (j in seq_along(count_cols)) {
    v <- format(m[, j], big.mark = ",", trim = TRUE)
    v[is.na(m[, j])] <- ""
    v[hide[, j]] <- "—"
    out[[count_cols[j]]] <- v
  }
  out
}

# share_mode では表示しない内容のための目印（チャンク内で使う）
sh_private <- function(expr, share = SHARE) {
  if (share) {
    cat("（共有用PDFでは、この内容は表示していません）\n")
    return(invisible(NULL))
  }
  expr
}

# 表の出力（LaTeX）。表題の _ % & # をエスケープ
sh_kable <- function(x, caption = NULL, digits = 3, ...) {
  if (!is.null(caption)) caption <- gsub("([_%&#])", "\\\\\\1", caption)
  knitr::kable(x, format = "latex", booktabs = TRUE, longtable = TRUE,
               caption = caption, digits = digits, linesep = "", ...)
}

# 共有用PDFの冒頭に入れる注記
sh_banner <- function(share = SHARE, min_cell = MIN_CELL) {
  if (share) {
    cat(sprintf("**共有用（share\\_mode = TRUE）**：件数が %d 未満のセル、およびその件数から計算した平均・割合は「—」で伏せています。最大値・最小値、個別の値、ID、範囲チェックは出力していません。\n", min_cell))
  } else {
    cat("**自分用（share\\_mode = FALSE）**：個別の確認用の表を含みます。この PDF は共有しないでください。\n")
  }
}
