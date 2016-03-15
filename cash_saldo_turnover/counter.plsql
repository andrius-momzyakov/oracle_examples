procedure recalc_saldo(errbuf                 out nocopy varchar2,
                         retcode                out nocopy number,
                         pd_beg_date            IN DATE, -- начало периода (включительно)
                         pd_end_date            IN DATE, -- конец периода (включительно)
                         pn_internal_account_id IN NUMBER, -- должн
                         pv_is1c                IN VARCHAR2, -- Y - источник 1с, N - источник БВ
                         pn_term                in NUMBER DEFAULT 1, -- временной горизонт поиска бв в месяцах
                         -- - по умолчанию не более 1 мес назад (задаётся со знаком + !)
                         pn_beg_saldo_in in NUMBER, -- входящее сальдо, заданное руками по запросу в формсах
                         pd_beg_date_in  in DATE, -- дата вх сальдо, заданного руками
                         pn_beg_saldo1   out NUMBER, -- самое "последнее" вх. сальдо из БВ, у которых дата нач совпадает с pn_beg_date -ВСЕГДА NULL с 13.03.2012 AMOMZYAKOV
                         pd_beg_date1    out DATE, -- дата сальдо 1 -ВСЕГДА NULL с 13.03.2012 AMOMZYAKOV
                         pn_beg_saldo2   out NUMBER, -- самое "последнее" вх. сальдо из БВ, у которых период включает с pn_beg_date
                         pd_beg_date2    out DATE, -- дата сальдо 1
                         pn_beg_saldo3   out NUMBER, -- самое "последнее" расч сальдо, у которого дата нач совпадает с pn_beg_date
                         pd_beg_date3    out DATE, -- дата сальдо 1
                         pv_check_account IN VARCHAR2 DEFAULT 'Y' -- доб AMOMZYAKOV 23/05/2012 - признак проверки внутреннего счёта
                                                                  -- для исключения двойной проверки при вызове из recalc_saldo_check_period
                         ) IS
    ln_input_saldo    xxpi_bk_imp_accf.beg_balance%TYPE;
    tmp               NUMBER;
    tmp1              NUMBER;
    tmp_date          DATE;
    vd_beg_date       DATE; -- начало периода
    vd_beg_date_in    DATE;
    vd_end_date       DATE; -- конец
    vn_start_balance  NUMBER;
    vn_beg_balance    NUMBER;
    vn_beg_balance_in NUMBER;
    vn_end_balance    NUMBER;
    vn_udebit         NUMBER;
    vn_ucredit        NUMBER;
    vn_osv_id         NUMBER;
    vn_file_id        NUMBER;
    R                 xxpi_bk_imp_accf%ROWTYPE;
    vd_day            DATE;
    is_saldo_found    BOOLEAN;
    vn_rowcount       NUMBER;
    counter           NUMBER;
    ERR_FLAG          VARCHAR2(10);
    vb_no_bkdata      VARCHAR2(1); -- для контроля наличия выписке на данный денёчек
    CURSOR internal_accid_cur IS -- для проверки типа переданного счёта
      select bank_account_id
        from ap_bank_accounts_all
       where bank_account_id = pn_internal_account_id
         and org_id = 81
         and account_type = 'INTERNAL';
    CURSOR days IS -- курсор по массиву дат периода
      select day
        from (select pd_beg_date + level - 1 day
                from dual
              connect by level <= pd_end_date - pd_beg_date + 1) q; -- подзапрос здесь необходим, без подзапроса такой курсор даёт ток одну запиь!!!
    -- TODO заменить select в days_wo_bk
    CURSOR days_wo_bk(p_begdate DATE, p_enddate DATE) IS --дни, не подтверждённые выпиской, для заданного периода
      select day
        from (select p_begdate + level - 1 day
                from dual
              connect by level <= p_enddate - p_begdate + 1) q
      MINUS
      select days.day
        from xxpi_bk_imp_file f,
             xxpi_bk_imp_accf a,
             (select p_begdate + level - 1 day
                from dual
              connect by level <= p_enddate - p_begdate + 1) days
       where 1 = 1
         AND days.day >= f.beg_date
         and days.day <= f.end_date
         and f.is_1c = pv_is1c --'N'
         and a.create_cd like '%F%'
         and a.file_id = f.file_id
         and a.bank_acc_id = pn_internal_account_id

      ;
    CURSOR lock_accf_cur IS
      select osv_id
        from xxpi_osv
       where bank_acc_id = pn_internal_account_id
         and bank_date >= pd_beg_date
         and bank_date <= pd_end_date
         and is_1c = pv_is1c;

  begin
    vb_no_bkdata := 'N'; -- полагаем, что выписко есть

    errbuf := 'recalc_saldo: ';

    -- проверяем вх параметры
    IF pd_beg_date IS NULL or pd_end_date IS NULL or
       pd_beg_date > pd_end_date THEN
      retcode := 2; -- ошибка с вылетом
      errbuf  := errbuf ||
                 'Некорректно задан диапазон дат. Пересчёт невозможен.';
      RETURN;
    end if;

    if (pd_beg_date_in IS NULL and pn_beg_saldo_in IS NOT NULL) then
      retcode := 2; -- ошибка с вылетом
      errbuf  := errbuf || 'Задано сальдо без даты. Пересчёт невозможен.';
      RETURN;
    end if;

    if (pn_beg_saldo_in IS NOT NULL and pd_beg_date_in IS NULL) then
      retcode := 2; -- ошибка с вылетом
      errbuf  := errbuf ||
                 'Задана дата сальдо без сальдо. Пересчёт невозможен.';
      RETURN;
    end if;

    if pv_check_account = 'Y' then -- проверка доб AMOMZYAKOV 23.05/2012
       if pn_internal_account_id IS NULL then
          retcode := 2; -- ошибка с вылетом
          errbuf  := errbuf ||
                 'Не задан ID внутреннего р.с. Пересчёт невозможен.';
          RETURN;
       else
          -- проверка, что счёт существует и является внутренним
          -- TODO добавить номер счёта и валюту
          open internal_accid_cur;
          fetch internal_accid_cur
                into tmp;
          if internal_accid_cur%NOTFOUND then
             retcode := 2;
             errbuf  := errbuf || 'Задан неверный ID внутреннего р.с.';
             close internal_accid_cur;
             RETURN;
          end if;
       end if;
    end if;

    if UPPER(pv_is1c) not in ('Y', 'N') then
      retcode := 2;
      errbuf  := errbuf || 'Неверно задан источник выписки.';
      RETURN;
    end if;

    -- определяем опорный вх. остаток по зад счёту, взятый из выписки, для периода - берём самую близлежащую по времени запись,
    -- предшествующую началу периода, унд не содержащую "разбегов" и берем ея END_BALANCE в качестве опорнаго вх остатка

    err_flag := 'step1'; -- этап наигрыша опорного начального сальдо для пересчёта
    if pn_beg_saldo_in IS NULL then
      /*
      -- AMOMZYAKOV 13/03/2012 ОТключено, так как 2-е сальдо возвращает то же
      begin
         -- 1-е сальдо - ищем последнюю БВ такую, что у нее нач. периода соотв pd_beg_date т.е. сальдо банка
         select a.beg_date, a.beg_balance
          into  pd_beg_date1, pn_beg_saldo1
          from xxpi_bk_imp_accf a
          where 1=1
                and bank_acc_id = pn_internal_account_id
                and create_cd like '%F%' -- загружено из БВ
                and is_1c = pv_is1c
                and beg_date = pd_beg_date
                and creation_date = (
                    select max(aa.creation_date)
                           from xxpi_bk_imp_accf aa
                           where 1=1
                                 and bank_acc_id = a.bank_acc_id
                                 and create_cd = a.create_cd
                                 and is_1c = a.is_1c
                                 -- and dubl = a.dubl
                                 and beg_date = pd_beg_date
                    )
          ;
      exception
          when others then NULL; -- просто не нашли сальдо
      end;
      */
      begin
        -- 2-е сальдо - ищем последнюю загруж выписку, период которой включает начало заданного периода
        -- и берём за основу ея входящее сальдо
        select a.beg_date, a.beg_balance
          into pd_beg_date2, pn_beg_saldo2
          from xxpi_bk_imp_accf a
         where 1 = 1
           and bank_acc_id = pn_internal_account_id --10004
           and create_cd like '%F%' -- загружено из БВ
           and is_1c = pv_is1c --'N'
              --and dubl = 'N'
              -- убирается, т.к. соотв данные в прошлых периодах могут быть неверными
              -- and not (prev_ok is not null and prev_ok = '*' )
              -- and not (next_ok is not null and next_ok = '*' )
              -- and not (vrf is not null and vrf = 'N')
           and beg_date <= pd_beg_date --to_date('20062011','ddmmyyyy')
           and end_date >= pd_beg_date -- выписки с концом периода не старше 1 месяца
           and creation_date = (select max(aa.creation_date)
                                  from xxpi_bk_imp_accf aa
                                 where 1 = 1
                                   and bank_acc_id = a.bank_acc_id
                                   and create_cd = a.create_cd
                                   and is_1c = a.is_1c
                                      -- and dubl = a.dubl
                                   and beg_date <= pd_beg_date --to_date('20062011','ddmmyyyy')
                                   and end_date >= pd_beg_date -- выписки с концом периода не старше 1 месяца
                                --and not (prev_ok is not null and prev_ok = '*' )
                                --and not (next_ok is not null and next_ok = '*' )
                                --and not (vrf is not null and vrf = 'N')
                                );
      exception
        when others then
          NULL;
      end;

      begin
        -- 3-е сальдо - расчитанное по алгоритму Шашковой при загрузке на день начала заданного периода
        select a.beg_date, beg_balance
          into pd_beg_date3, pn_beg_saldo3
          from xxpi_bk_imp_accf a
         where 1 = 1
           and bank_acc_id = pn_internal_account_id
           and create_cd like '%D%' -- расчётное значение
           and is_1c = pv_is1c
           and beg_date = pd_beg_date
           and creation_date =
               (select max(aa.creation_date)
                  from xxpi_bk_imp_accf aa
                 where 1 = 1
                   and bank_acc_id = a.bank_acc_id
                   and create_cd = a.create_cd
                   and is_1c = a.is_1c
                      -- and dubl = a.dubl
                   and beg_date = pd_beg_date);
      exception
        when OTHERS then
          NULL;
      end;

      retcode := 1;
      errbuf  := err_flag;
      RETURN; -- возвращаем сальдо в форму для выбора или ввода другого отличного значения подтверждения
    else
      vn_beg_balance_in := pn_beg_saldo_in; -- вх сальдо , заданное руками
      vd_beg_date_in    := pd_beg_date_in; -- дата вх сальдо, заданного руками

      -- проверяем, все ли дни загружены в период с pd_beg_date_in по pd_beg_date или наоборот,
      -- в зависимости какая дата более ранняя

      /*
      -- AMOMZYAKOV 13.03.2012: Проверка отключена, т.к. необходимо включать в ведомость все дни, по которым нет выписки.
      if pd_beg_date_in > pd_beg_date then
         open days_wo_bk(pd_beg_date, pd_beg_date_in);
      else
         open days_wo_bk(pd_beg_date_in, pd_beg_date);
      end if;

      fetch days_wo_bk into tmp_date;
      if days_wo_bk%FOUND then
         retcode := 2;
         errbuf := errbuf || 'Между датой начала периода и заданной датой вх сальдо есть дни, не подтвержденные выпиской из банка.'||
                          ' Необходимо сначала загрузить все дни или выбрать другое сальдо или период расчёта.';
         close days_wo_bk;
         RETURN;
      end if;
      */

      -- вычисляем вх сальдо для периода расчёта
      if pd_beg_date_in > pd_beg_date then
        -- считаем "назад", если дата вх сальдо более поздняя, чем дата начала периода
        select -nvl(sum(debit), 0) d, -nvl(sum(credit), 0) c
          into vn_udebit, vn_ucredit
          from xxpi_bk_imp_udoc
         where 1 = 1
           and bank_acc_id = pn_internal_account_id --10004
           and bank_date >= pd_beg_date
           and bank_date < pd_beg_date_in -- < т.к. пляшем от входящего сальдо
           and deleted_flg = 'N'
           and is_1c = pv_is1c
           and (request_id is null or request_id = 0); -- только окончательно загруженные;
      else
        -- считаем вперёд
        select nvl(sum(debit), 0) d, nvl(sum(credit), 0) c
          into vn_udebit, vn_ucredit
          from xxpi_bk_imp_udoc
         where 1 = 1
           and bank_acc_id = pn_internal_account_id --10004
           and bank_date >= pd_beg_date_in
           and bank_date < pd_beg_date -- < т.к. пляшем от входящего сальдо
           and deleted_flg = 'N'
           and is_1c = pv_is1c
           and (request_id is null or request_id = 0); -- только окончательно загруженные;
      end if;

      vn_beg_balance := vn_beg_balance_in + vn_ucredit - vn_udebit;

    end if; -- проверка pn_beg_saldo_in

    err_flag := 'step2'; --последующие шаги

    -- блокировка записей accf для счёта и периода

    open lock_accf_cur;

    --и обновление записей в xxpi_bk_imp_accf для периода

    open days;
    vn_rowcount := 0;
    counter     := 0;
    while true loop
      fetch days
        into vd_day;
      exit when days%NOTFOUND;
      counter        := counter + 1;
      is_saldo_found := FALSE;

      BEGIN
        select osv_id
          into vn_osv_id
          from xxpi_osv o
         where 1 = 1
           and vd_day = o.bank_date
           and o.bank_acc_id = pn_internal_account_id
           and o.is_1c = pv_is1c;

        /*select a.accf_id into tmp
        from xxpi_bk_imp_accf a
        where 1=1
              and vd_day = a.beg_date
              and a.beg_date = a.end_date
              and a.bank_acc_id = pn_internal_account_id
              and a.dubl = 'N'
              and a.is_1c = pv_is1c
              and a.beg_date >= pd_beg_date
              and a.end_date <= pd_end_date;
              */

        is_saldo_found := TRUE;
      EXCEPTION
        when NO_DATA_FOUND then
          is_saldo_found := FALSE;
          /*            when TOO_MANY_ROWS then
          is_saldo_found := TRUE; -- в принципе не должно быть
          */
        when OTHERS then
          retcode := 2;
          errbuf  := errbuf || 'ex1: ' || SQLERRM;
          close lock_accf_cur;
          close days;
          rollback;
          RETURN;
      END;

      -- считаем обороты за тек день
      select nvl(sum(debit), 0) d, nvl(sum(credit), 0) c
        into vn_udebit, vn_ucredit
        from xxpi_bk_imp_udoc
       where 1 = 1
         and bank_acc_id = pn_internal_account_id --10004
         and bank_date = vd_day
         and deleted_flg = 'N'
         and is_1c = pv_is1c
         and (request_id is null or request_id = 0);

      -- доб AMOMZYAKOV 16/02/2012
      -- признак отстуствия выписки на день - для вставки NULL в udebit_null и ucredit_null,
      -- где отсутствует выписко. иначе вставляются значения полей или заливных лугов udebit и ucredit

      /* переделал AMOMZYAKOV 27/02/2012 - для послед. записи id актуальных на момент расчёта
      -- оборотов унд сальдо выписог, чтобы неактуальные обороты выделить красным в форме
      begin
         select 'N' into vb_no_bkdata -- выписко есть!
                from xxpi_bk_imp_accf
                     where 1=1
                           and bank_acc_id = pn_internal_account_id
                           and is_1c = pv_is1c
                           and beg_date <= vd_day
                           and end_date >= vd_day
                           and create_cd like '%F%'
                           and rownum = 1;

      exception
         when OTHERS then vb_no_bkdata := 'Y'; -- выписко нетути !
      end;
      */

      begin
        -- ищем id БВ по табличке ACCF - для некоторых выписок тупых банков может и не найти!
        select 'N', file_id
          into vb_no_bkdata, vn_file_id -- выписко как-бы есть!
          from xxpi_bk_imp_accf a
         where 1 = 1
           and a.bank_acc_id = pn_internal_account_id
           and a.is_1c = pv_is1c
           and a.beg_date <= vd_day
           and a.end_date >= vd_day
           and a.create_cd like '%F%'
           and a.creation_date =
               (select max(b.creation_date)
                  from xxpi_bk_imp_accf b
                 where 1 = 1
                   and b.bank_acc_id = a.bank_acc_id
                   and b.is_1c = a.is_1c
                   and beg_date <= vd_day
                   and end_date >= vd_day
                   and create_cd like '%F%');
        /* конец доработки */
      exception
        when OTHERS then
          vb_no_bkdata := 'Y';
          vn_file_id   := NULL; -- выписко нетути !
      end;
      -- конец 16.02.2012

      -- AMOMZYAKOV 29/02/2012 Доб. поиск выписки для случаев битых выписок тупых банков,
      -- где забывают в некоторые дни вписать инфу о сальдо и оборотах по р.с.
      if vn_file_id IS NULL then
        begin
          select distinct 'N', f.file_id
            into vb_no_bkdata, vn_file_id -- выписко как-бы есть!
            from xxpi_bk_imp_accf a, xxpi_bk_imp_file f
           where 1 = 1
             and f.file_id = a.file_id
             and a.bank_acc_id = pn_internal_account_id
             and a.is_1c = pv_is1c --'N'
             and f.beg_date <= vd_day
             and f.end_date >= vd_day
             and a.create_cd like '%F%'
             and f.creation_date =
                 (select max(f1.creation_date)
                    from xxpi_bk_imp_file f1, xxpi_bk_imp_accf a1
                   where 1 = 1
                     and f1.file_id = a1.file_id
                     and a1.bank_acc_id = a.bank_acc_id
                     and a1.is_1c = a.is_1c
                     and f1.beg_date <= vd_day
                     and f1.end_date >= vd_day
                     and a1.create_cd like '%F%');

        exception
          when OTHERS then
            vb_no_bkdata := 'Y';
            vn_file_id   := NULL; -- выписко нетути !
        end;
      end if;

      --конец 29.02.2012

      -- обновляем остатки за тек день
      vn_end_balance := vn_beg_balance + vn_ucredit - vn_udebit;

      IF is_saldo_found THEN
        -- обновляем найденную строку с остатками
        update xxpi_osv o --xxpi_bk_imp_accf
           set file_id      = vn_file_id,
               beg_balance  = vn_beg_balance,
               end_balance  = vn_end_balance,
               udebit       = vn_udebit,
               udebit_null  = DECODE(vb_no_bkdata, 'Y', NULL, vn_udebit),
               ucredit      = vn_ucredit,
               ucredit_null = DECODE(vb_no_bkdata, 'Y', NULL, vn_ucredit),
               update_date  = SYSDATE,
               updated_by   = n_user_id,
               beg_date     = pd_beg_date,
               end_date     = pd_end_date
         where 1 = 1
           and vn_osv_id = osv_id;

        vn_rowcount := vn_rowcount + sql%ROWCOUNT;
      ELSE
        -- добавляем остатки, если не нашли запись
        select xxpi_osv_seq.NEXTVAL into tmp from DUAL;
        insert into xxpi_osv
          (osv_id,
           bank_acc_id,
           bank_date,
           is_1c,
           is_1c_reversed,
           beg_balance,
           end_balance,
           udebit,
           udebit_null,
           ucredit,
           ucredit_null,
           file_id,
           created_by,
           creation_date,
           beg_date,
           end_date)
        values
          (tmp,
           pn_internal_account_id,
           vd_day,
           pv_is1c,
           DECODE(pv_is1c, 'Y', 'N', 'Y'),
           vn_beg_balance,
           vn_end_balance,
           vn_udebit,
           DECODE(vb_no_bkdata, 'Y', NULL, vn_udebit),
           vn_ucredit,
           DECODE(vb_no_bkdata, 'Y', NULL, vn_ucredit),
           vn_file_id,
           n_user_id,
           SYSDATE,
           pd_beg_date,
           pd_end_date);
      END IF;
      vn_beg_balance := vn_end_balance;
      --vn_rowcount := vn_rowcount + 1;
    end loop;
    errbuf := errbuf || counter || 'итераций. ' || 'Успешно обновлено ' ||
              vn_rowcount || ' записей.';
    commit;
    close days;
    close lock_accf_cur;

    retcode := 0;

  EXCEPTION
    when others then
      if days%ISOPEN then
        close days;
      end if;
      /*
      if days_wo_bk%ISOPEN then
         close days_wo_bk;
      end if;
      */
      if internal_accid_cur%ISOPEN then
        close internal_accid_cur;
      end if;
      if lock_accf_cur%ISOPEN then
        close lock_accf_cur;
      end if;
      rollback;
      retcode := 2;
      if err_flag = 'step1' then
        -- не нашли подходящее входящее сальдо
        errbuf := err_flag;
      else
        errbuf := errbuf || 'ex2: ' || SQLERRM;
      end if;

  end recalc_saldo;
